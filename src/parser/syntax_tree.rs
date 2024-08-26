use std::fmt::Debug;
use std::fmt::Formatter;
use std::str::FromStr;

use num_bigint::BigInt;
use pest::iterators::Pair;
use pest::Span;

use crate::parser::peg_grammar::Rule;

pub struct SpanPair {
    pub start: usize,
    pub end: usize,
}

impl Debug for SpanPair {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.start, self.end)
    }
}

impl From<Span<'_>> for SpanPair {
    fn from(span: Span) -> Self {
        SpanPair {
            start: span.start(),
            end: span.end(),
        }
    }
}

#[derive(Debug)]
pub enum TypeRef {
    TypeName(Identifier),
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
    pub span: SpanPair,
}

#[derive(Debug)]
pub struct Program {
    pub decls: Vec<TopDecl>,
    pub span: SpanPair,
}

#[derive(Debug)]
pub enum TopDecl {
    Func(Function),
}

#[derive(Debug)]
pub struct Function {
    pub ident: Identifier,
    pub params: Vec<FuncParam>,
    pub return_type: TypeRef,
    pub stmts: Vec<Stmt>,
    pub span: SpanPair,
}

#[derive(Debug)]
pub struct FuncParam {
    pub ident: Identifier,
    pub type_ref: TypeRef,
    pub span: SpanPair,
}

#[derive(Debug)]
pub enum Stmt {
    Empty(SpanPair),
    Let(LetStmt),
    Return(ReturnStmt),
    If(IfStmt),
    Expr(ExprStmt),
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub expr: Expr,
    pub span: SpanPair,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Expr,
    pub span: SpanPair,
}

#[derive(Debug)]
pub struct LetStmt {
    pub ident: Identifier,
    pub type_ref: TypeRef,
    pub expr: Expr,
    pub span: SpanPair,
}

#[derive(Debug)]
pub struct IfStmt {
    pub test: Expr,
    pub then_statements: Vec<Stmt>,
    pub else_statements: Else,
    pub span: SpanPair,
}

#[derive(Debug)]
pub enum Else {
    ElseIf(Box<IfStmt>),
    ElseStatements(Vec<Stmt>),
    Empty(SpanPair),
}

#[derive(Debug)]
pub struct Expr {
    pub expr: ExprType,
    pub span: SpanPair,
}

#[derive(Debug)]
pub enum ExprType {
    IntLiteral(BigInt),
    BoolLiteral(bool),
    VarReference(String),
    Add(Vec<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Vec<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Negate(Box<Expr>),
    Paren(Box<Expr>),
}

pub fn program(pair: Pair<Rule>) -> Program {
    let mut decls = vec![];
    let span = pair.as_span().into();
    for child in pair.into_inner() {
        match child.as_rule() {
            Rule::topdecl => decls.push(top_decl(child)),
            Rule::EOI => {}
            r => {
                panic!("unexpected program rule {:?}", r);
            }
        };
    }
    Program { decls, span }
}

fn top_decl(pair: Pair<Rule>) -> TopDecl {
    TopDecl::Func(function(pair.into_inner().next().unwrap()))
}

fn function(pair: Pair<Rule>) -> Function {
    let span = pair.as_span().into();
    let mut children = pair.into_inner();
    let ident = identifier(children.next().unwrap());
    let mut next = children.next().unwrap();
    let params = if next.as_rule() == Rule::func_params {
        let params = next;
        next = children.next().unwrap();
        func_params(params)
    } else {
        vec![]
    };
    let return_type = type_reference(next);
    let stmts = statement_block(children.next().unwrap());
    Function {
        ident,
        params,
        return_type,
        stmts,
        span,
    }
}

fn func_params(pair: Pair<Rule>) -> Vec<FuncParam> {
    pair.into_inner().map(func_param).collect()
}

fn func_param(pair: Pair<Rule>) -> FuncParam {
    let span = pair.as_span().into();
    let mut children = pair.into_inner();
    let ident = identifier(children.next().unwrap());
    let type_ref = type_reference(children.next().unwrap());
    FuncParam {
        ident,
        type_ref,
        span,
    }
}

fn type_reference(pair: Pair<Rule>) -> TypeRef {
    TypeRef::TypeName(identifier(pair.into_inner().next().unwrap()))
}

fn statement_block(pair: Pair<Rule>) -> Vec<Stmt> {
    pair.into_inner().map(statement).collect()
}

fn statement(pair: Pair<Rule>) -> Stmt {
    let child = pair.into_inner().next().unwrap();
    match child.as_rule() {
        Rule::empty_stmt => Stmt::Empty(child.as_span().into()),
        Rule::let_stmt => Stmt::Let(let_statement(child)),
        Rule::return_stmt => Stmt::Return(return_statement(child)),
        Rule::expr_stmt => Stmt::Expr(expr_statement(child)),
        r => panic!("unexpected statement rule {:?}", r),
    }
}

fn let_statement(pair: Pair<Rule>) -> LetStmt {
    let span = pair.as_span().into();
    let mut children = pair.into_inner();
    let ident = identifier(children.next().unwrap());
    let type_ref = type_reference(children.next().unwrap());
    let expr = expression(children.next().unwrap());
    LetStmt {
        ident,
        type_ref,
        expr,
        span,
    }
}

fn return_statement(pair: Pair<Rule>) -> ReturnStmt {
    let span = pair.as_span().into();
    let expr = expression(pair.into_inner().next().unwrap());
    ReturnStmt { expr, span }
}

fn expr_statement(pair: Pair<Rule>) -> ExprStmt {
    let span = pair.as_span().into();
    let expr = expression(pair.into_inner().next().unwrap());
    ExprStmt { expr, span }
}

fn expression(mut pair: Pair<Rule>) -> Expr {
    let span = pair.as_span().into();
    if pair.as_rule() == Rule::expr {
        pair = pair.into_inner().next().unwrap();
    }
    let expr = match pair.as_rule() {
        Rule::int_literal => int_literal(pair),
        Rule::addops => add_operator(pair),
        Rule::identifier => var_reference(pair),
        Rule::binop => binary_operator(pair),
        Rule::unary => unary_operator(pair),
        Rule::paren => paren(pair),
        r => panic!("unexpected expression rule {:?}", r),
    };
    Expr { expr, span }
}

fn paren(pair: Pair<Rule>) -> ExprType {
    ExprType::Paren(Box::new(expression(pair.into_inner().next().unwrap())))
}

fn int_literal(pair: Pair<Rule>) -> ExprType {
    ExprType::IntLiteral(
        BigInt::from_str(pair.as_span().as_str()).expect("invalid integer literal"),
    )
}

fn var_reference(pair: Pair<Rule>) -> ExprType {
    ExprType::VarReference(String::from(pair.as_span().as_str()))
}

fn add_operator(pair: Pair<Rule>) -> ExprType {
    ExprType::Add(pair.into_inner().map(expression).collect())
}

fn binary_operator(pair: Pair<Rule>) -> ExprType {
    let mut children = pair.into_inner();
    let lhs = expression(children.next().unwrap());
    let op = children.next().unwrap().as_rule();
    let rhs = expression(children.next().unwrap());
    match op {
        Rule::div => ExprType::Div(Box::new(lhs), Box::new(rhs)),
        r => panic!("unexpected binary operator {:?}", r),
    }
}

fn unary_operator(pair: Pair<Rule>) -> ExprType {
    let mut children = pair.into_inner();
    let op = children.next().unwrap().as_rule();
    let term = expression(children.next().unwrap());
    match op {
        Rule::negative => ExprType::Negate(Box::new(term)),
        r => panic!("unexpected unary operator {:?}", r),
    }
}

fn identifier(pair: Pair<Rule>) -> Identifier {
    let span = pair.as_span().into();
    let name = String::from(pair.as_span().as_str());
    Identifier { name, span }
}
