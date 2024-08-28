use std::fmt::Debug;
use std::fmt::Formatter;
use std::str::FromStr;

use num_bigint::BigInt;
use pest::iterators::Pair;
use pest::Span;

use crate::error::PanicErrorImpl;
use crate::error::PanicLangError;
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
    Func(FunctionDecl),
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub ident: Identifier,
    pub params: Vec<FuncParamDecl>,
    pub return_type: TypeRef,
    pub stmts: Vec<Stmt>,
    pub span: SpanPair,
}

#[derive(Debug)]
pub struct FuncParamDecl {
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
    Empty(),
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
    VarReference(Identifier),
    FuncCall(Identifier, Vec<Expr>),
    Add(Vec<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Vec<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Negate(Box<Expr>),
    Paren(Box<Expr>),
}

pub fn program(pair: Pair<Rule>) -> Result<Program, PanicLangError> {
    let mut decls = vec![];
    let span = pair.as_span().into();
    for child in pair.into_inner() {
        match child.as_rule() {
            Rule::topdecl => decls.push(top_decl(child)?),
            Rule::EOI => {}
            r => {
                return PanicErrorImpl::SyntaxTreeError(format!("unexpected program rule {:?}", r))
                    .into();
            }
        };
    }
    Ok(Program { decls, span })
}

fn top_decl(pair: Pair<Rule>) -> Result<TopDecl, PanicLangError> {
    Ok(TopDecl::Func(function_decl(
        pair.into_inner().next().unwrap(),
    )?))
}

fn function_decl(pair: Pair<Rule>) -> Result<FunctionDecl, PanicLangError> {
    let span = pair.as_span().into();
    let mut children = pair.into_inner();
    let ident = identifier(children.next().unwrap())?;
    let mut next = children.next().unwrap();
    let params = if next.as_rule() == Rule::func_params {
        let params = next;
        next = children.next().unwrap();
        func_params_decl(params)?
    } else {
        vec![]
    };
    let return_type = type_reference(next)?;
    let stmts = statement_block(children.next().unwrap())?;
    Ok(FunctionDecl {
        ident,
        params,
        return_type,
        stmts,
        span,
    })
}

fn func_params_decl(pair: Pair<Rule>) -> Result<Vec<FuncParamDecl>, PanicLangError> {
    pair.into_inner()
        .map(func_param_decl)
        .collect::<Result<Vec<_>, _>>()
}

fn func_param_decl(pair: Pair<Rule>) -> Result<FuncParamDecl, PanicLangError> {
    let span = pair.as_span().into();
    let mut children = pair.into_inner();
    let ident = identifier(children.next().unwrap())?;
    let type_ref = type_reference(children.next().unwrap())?;
    Ok(FuncParamDecl {
        ident,
        type_ref,
        span,
    })
}

fn type_reference(pair: Pair<Rule>) -> Result<TypeRef, PanicLangError> {
    Ok(TypeRef::TypeName(identifier(
        pair.into_inner().next().unwrap(),
    )?))
}

fn statement_block(pair: Pair<Rule>) -> Result<Vec<Stmt>, PanicLangError> {
    pair.into_inner()
        .map(statement)
        .collect::<Result<Vec<_>, _>>()
}

fn statement(pair: Pair<Rule>) -> Result<Stmt, PanicLangError> {
    let child = pair.into_inner().next().unwrap();
    match child.as_rule() {
        Rule::empty_stmt => Ok(Stmt::Empty(child.as_span().into())),
        Rule::let_stmt => Ok(Stmt::Let(let_statement(child)?)),
        Rule::if_stmt => Ok(Stmt::If(if_statement(child)?)),
        Rule::return_stmt => Ok(Stmt::Return(return_statement(child)?)),
        Rule::expr_stmt => Ok(Stmt::Expr(expr_statement(child)?)),
        r => PanicErrorImpl::SyntaxTreeError(format!("unexpected statement rule {:?}", r)).into(),
    }
}

fn let_statement(pair: Pair<Rule>) -> Result<LetStmt, PanicLangError> {
    let span = pair.as_span().into();
    let mut children = pair.into_inner();
    let ident = identifier(children.next().unwrap())?;
    let type_ref = type_reference(children.next().unwrap())?;
    let expr = expression(children.next().unwrap())?;
    Ok(LetStmt {
        ident,
        type_ref,
        expr,
        span,
    })
}

fn if_statement(pair: Pair<Rule>) -> Result<IfStmt, PanicLangError> {
    let span = pair.as_span().into();
    let mut children = pair.into_inner();
    let test = expression(children.next().unwrap())?;
    let then_statements = statement_block(children.next().unwrap())?;
    let child = children.next();
    let rule = child.as_ref().map(|p| p.as_rule());
    let else_statements = match rule {
        None => Ok(Else::Empty()),
        Some(Rule::stmt_block) => Ok(Else::ElseStatements(statement_block(child.unwrap())?)),
        Some(Rule::if_stmt) => Ok(Else::ElseIf(Box::new(if_statement(child.unwrap())?))),
        Some(r) => {
            PanicErrorImpl::SyntaxTreeError(format!("unexpected else if rule {:?}", r)).into()
        }
    }?;
    Ok(IfStmt {
        span,
        test,
        then_statements,
        else_statements,
    })
}

fn return_statement(pair: Pair<Rule>) -> Result<ReturnStmt, PanicLangError> {
    let span = pair.as_span().into();
    let expr = expression(pair.into_inner().next().unwrap())?;
    Ok(ReturnStmt { expr, span })
}

fn expr_statement(pair: Pair<Rule>) -> Result<ExprStmt, PanicLangError> {
    let span = pair.as_span().into();
    let expr = expression(pair.into_inner().next().unwrap())?;
    Ok(ExprStmt { expr, span })
}

fn expression(mut pair: Pair<Rule>) -> Result<Expr, PanicLangError> {
    let span = pair.as_span().into();
    if pair.as_rule() == Rule::expr {
        pair = pair.into_inner().next().unwrap();
    }
    let expr = match pair.as_rule() {
        Rule::int_literal => int_literal(pair),
        Rule::bool_literal => bool_literal(pair),
        Rule::addops => add_operator(pair),
        Rule::mulops => mul_operator(pair),
        Rule::identifier => var_reference(pair),
        Rule::binop => binary_operator(pair),
        Rule::unary => unary_operator(pair),
        Rule::paren => paren(pair),
        Rule::func_call => function_call(pair),
        r => PanicErrorImpl::SyntaxTreeError(format!("unexpected expression rule {:?}", r)).into(),
    }?;
    Ok(Expr { expr, span })
}

fn paren(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    Ok(ExprType::Paren(Box::new(expression(
        pair.into_inner().next().unwrap(),
    )?)))
}

fn int_literal(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    Ok(ExprType::IntLiteral(
        BigInt::from_str(pair.as_span().as_str()).map_err(|e| {
            PanicErrorImpl::SyntaxTreeError(format!("invalid integer literal: {e}"))
        })?,
    ))
}

fn bool_literal(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    Ok(ExprType::BoolLiteral(
        bool::from_str(pair.as_span().as_str()).map_err(|e| {
            PanicErrorImpl::SyntaxTreeError(format!("invalid boolean literal: {e}"))
        })?,
    ))
}

fn var_reference(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    Ok(ExprType::VarReference(identifier(pair)?))
}

fn add_operator(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    Ok(ExprType::Add(
        pair.into_inner()
            .map(expression)
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

fn mul_operator(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    Ok(ExprType::Mul(
        pair.into_inner()
            .map(expression)
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

fn binary_operator(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    let mut children = pair.into_inner();
    let lhs = expression(children.next().unwrap())?;
    let op = children.next().unwrap().as_rule();
    let rhs = expression(children.next().unwrap())?;
    match op {
        Rule::sub => Ok(ExprType::Sub(Box::new(lhs), Box::new(rhs))),
        Rule::div => Ok(ExprType::Div(Box::new(lhs), Box::new(rhs))),
        r => PanicErrorImpl::SyntaxTreeError(format!("unexpected binary operator {:?}", r)).into(),
    }
}

fn unary_operator(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    let mut children = pair.into_inner();
    let op = children.next().unwrap().as_rule();
    let term = expression(children.next().unwrap())?;
    match op {
        Rule::negate => Ok(ExprType::Negate(Box::new(term))),
        r => PanicErrorImpl::SyntaxTreeError(format!("unexpected unary operator {:?}", r)).into(),
    }
}

fn function_call(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    let mut children = pair.into_inner();
    let iden = identifier(children.next().unwrap())?;
    let exprs = children.map(expression).collect::<Result<Vec<_>, _>>()?;
    Ok(ExprType::FuncCall(iden, exprs))
}

fn identifier(pair: Pair<Rule>) -> Result<Identifier, PanicLangError> {
    let span = pair.as_span().into();
    let name = String::from(pair.as_span().as_str());
    Ok(Identifier { name, span })
}

#[cfg(test)]
mod tests {
    use std::io::Read;

    use crate::parser::peg_grammar::PanicParser;

    use super::*;

    #[test]
    fn test_valid() {
        let path = "tests/valid";
        let entries = std::fs::read_dir(path).unwrap();
        for entry in entries {
            let entry = entry.expect("error reading directory");
            let mut file = std::fs::File::open(entry.path()).expect("error opening file");
            let mut input = Vec::new();
            file.read_to_end(&mut input).expect("error reading file");
            let input = String::from_utf8(input).expect("error converting file to string");
            let mut pairs = <PanicParser as pest::Parser<_>>::parse(Rule::program, &input)
                .expect("error parsing file");
            let top_node = pairs.next().unwrap();
            assert!(program(top_node).is_ok());
        }
    }

    #[test]
    fn test_invalid() {
        let path = "tests/invalid";
        let entries = std::fs::read_dir(path).unwrap();
        for entry in entries {
            let entry = entry.expect("error reading directory");
            let mut file = std::fs::File::open(entry.path()).expect("error opening file");
            let mut input = Vec::new();
            file.read_to_end(&mut input).expect("error reading file");
            let input = String::from_utf8(input).expect("error converting file to string");
            let pairs = <PanicParser as pest::Parser<_>>::parse(Rule::program, &input);
            let result = pairs.map(|mut p| program(p.next().unwrap()));
            assert!(result.is_err());
        }
    }
}
