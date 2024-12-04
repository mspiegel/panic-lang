use std::fmt::Debug;
use std::fmt::Formatter;
use std::str::FromStr;
use std::sync::Arc;

use num_bigint::BigInt;
use pest::iterators::Pair;
use pest::Span;

use crate::error::PanicErrorImpl;
use crate::error::PanicLangError;
use crate::parser::peg_grammar::Rule;

#[derive(Clone, Copy)]
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
pub struct TypeExpr {
    pub typ: TypeExprEnum,
    pub span: SpanPair,
}

#[derive(Debug)]
pub enum TypeExprEnum {
    Ref(TypeRef),
    Union(Vec<TypeRef>),
}

#[derive(Debug)]
pub struct DeclExpr {
    pub decl: DeclExprEnum,
    pub span: SpanPair,
}

#[derive(Debug)]
pub enum DeclExprEnum {
    Ref(DeclRef),
    Intersection(Vec<DeclRef>),
}

impl DeclExprEnum {
    pub fn to_vec(&self) -> Vec<DeclRef> {
        match self {
            DeclExprEnum::Ref(decl_ref) => vec![decl_ref.clone()],
            DeclExprEnum::Intersection(vec) => vec.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeRef {
    TypeName(Identifier),
}

#[derive(Clone, Debug)]
pub enum DeclRef {
    TypeName(Identifier),
}

impl DeclRef {
    pub fn identifier(&self) -> &Identifier {
        match self {
            DeclRef::TypeName(ident) => ident,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: Arc<String>,
    pub span: SpanPair,
}

#[derive(Debug)]
pub struct Program {
    pub decls: Vec<Decl>,
    pub span: SpanPair,
}

#[derive(Debug)]
pub enum Decl {
    Func(FunctionDecl),
    PrimitiveType(PrimitiveTypeDecl),
}

impl Decl {
    pub fn identifier(&self) -> &Identifier {
        match self {
            Decl::Func(func) => &func.ident,
            Decl::PrimitiveType(typ) => &typ.ident,
        }
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub ident: Identifier,
    pub params: Vec<FuncParamDecl>,
    pub return_type: TypeExpr,
    pub stmts: Vec<Stmt>,
    pub span: SpanPair,
}

#[derive(Debug)]
pub struct PrimitiveTypeDecl {
    pub ident: Identifier,
    pub relations: Option<DeclExpr>,
    pub span: SpanPair,
}

#[derive(Debug)]
pub struct FuncParamDecl {
    pub ident: Identifier,
    pub type_expr: TypeExpr,
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
    pub type_expr: TypeExpr,
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
    IntLiteral(Arc<BigInt>),
    BoolLiteral(bool),
    VarReference(Identifier),
    FuncCall(Identifier, Vec<Expr>),
    Add(Vec<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Vec<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    LtEq(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    NotEq(Box<Expr>, Box<Expr>),
    GtEq(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Negate(Box<Expr>),
    Not(Box<Expr>),
    Question(Box<Expr>),
    Paren(Box<Expr>),
}

pub fn program(pair: Pair<Rule>) -> Result<Program, PanicLangError> {
    let mut decls = vec![];
    let span = pair.as_span().into();
    for child in pair.into_inner() {
        match child.as_rule() {
            Rule::decl => decls.push(decl(child)?),
            Rule::EOI => {}
            r => {
                return PanicErrorImpl::SyntaxTreeError(format!("unexpected program rule {:?}", r))
                    .into();
            }
        };
    }
    Ok(Program { decls, span })
}

fn decl(pair: Pair<Rule>) -> Result<Decl, PanicLangError> {
    let child = pair.into_inner().next().unwrap();
    match child.as_rule() {
        Rule::fn_decl => Ok(Decl::Func(function_decl(child)?)),
        Rule::type_decl => type_decl(child),
        r => PanicErrorImpl::SyntaxTreeError(format!("unexpected declaration {:?}", r)).into(),
    }
}

fn type_decl(pair: Pair<Rule>) -> Result<Decl, PanicLangError> {
    let child = pair.into_inner().next().unwrap();
    match child.as_rule() {
        Rule::prim_decl => Ok(Decl::PrimitiveType(primitive_type_decl(child)?)),
        r => PanicErrorImpl::SyntaxTreeError(format!("unexpected type declaration {:?}", r)).into(),
    }
}

fn primitive_type_decl(pair: Pair<Rule>) -> Result<PrimitiveTypeDecl, PanicLangError> {
    let span = pair.as_span().into();
    let mut children = pair.into_inner();
    let ident = identifier(children.next().unwrap())?;
    let relations = match children.next() {
        Some(pair) => Some(decl_expression(pair.into_inner().next().unwrap())?),
        None => None,
    };
    Ok(PrimitiveTypeDecl {
        ident,
        relations,
        span,
    })
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
    let return_type = type_expression(next)?;
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
    let type_expr = type_expression(children.next().unwrap())?;
    Ok(FuncParamDecl {
        ident,
        type_expr,
        span,
    })
}

fn type_expression(pair: Pair<Rule>) -> Result<TypeExpr, PanicLangError> {
    let span = pair.as_span().into();
    let mut children = pair.into_inner();
    let child = children.next().unwrap();
    let typ = match child.as_rule() {
        Rule::type_union => Ok(TypeExprEnum::Union(type_union(child)?)),
        Rule::type_ref => Ok(TypeExprEnum::Ref(type_reference(child)?)),
        r => PanicErrorImpl::SyntaxTreeError(format!("unexpected type expression rule {:?}", r))
            .into(),
    }?;
    Ok(TypeExpr { typ, span })
}

fn type_union(pair: Pair<Rule>) -> Result<Vec<TypeRef>, PanicLangError> {
    pair.into_inner()
        .map(type_reference)
        .collect::<Result<Vec<_>, _>>()
}

fn type_reference(pair: Pair<Rule>) -> Result<TypeRef, PanicLangError> {
    Ok(TypeRef::TypeName(identifier(
        pair.into_inner().next().unwrap(),
    )?))
}

fn decl_expression(pair: Pair<Rule>) -> Result<DeclExpr, PanicLangError> {
    let span = pair.as_span().into();
    let mut children = pair.into_inner();
    let child = children.next().unwrap();
    let decl = match child.as_rule() {
        Rule::decl_isect => Ok(DeclExprEnum::Intersection(decl_intersection(child)?)),
        Rule::decl_ref => Ok(DeclExprEnum::Ref(decl_reference(child)?)),
        r => PanicErrorImpl::SyntaxTreeError(format!("unexpected decl expression rule {:?}", r))
            .into(),
    }?;
    Ok(DeclExpr { decl, span })
}

fn decl_intersection(pair: Pair<Rule>) -> Result<Vec<DeclRef>, PanicLangError> {
    pair.into_inner()
        .map(decl_reference)
        .collect::<Result<Vec<_>, _>>()
}

fn decl_reference(pair: Pair<Rule>) -> Result<DeclRef, PanicLangError> {
    Ok(DeclRef::TypeName(identifier(
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
    let type_expr = type_expression(children.next().unwrap())?;
    let expr = expression(children.next().unwrap())?;
    Ok(LetStmt {
        ident,
        type_expr,
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
        Rule::andops => and_operator(pair),
        Rule::orops => or_operator(pair),
        Rule::identifier => var_reference(pair),
        Rule::binop => binary_operator(pair),
        Rule::prefix => prefix_operator(pair),
        Rule::suffix => suffix_operator(pair),
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
    Ok(ExprType::IntLiteral(Arc::new(
        BigInt::from_str(pair.as_span().as_str()).map_err(|e| {
            PanicErrorImpl::SyntaxTreeError(format!("invalid integer literal: {e}"))
        })?,
    )))
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

fn and_operator(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    Ok(ExprType::And(
        pair.into_inner()
            .map(expression)
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

fn or_operator(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    Ok(ExprType::Or(
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
        Rule::lt => Ok(ExprType::Lt(Box::new(lhs), Box::new(rhs))),
        Rule::le => Ok(ExprType::LtEq(Box::new(lhs), Box::new(rhs))),
        Rule::eq => Ok(ExprType::Eq(Box::new(lhs), Box::new(rhs))),
        Rule::ne => Ok(ExprType::NotEq(Box::new(lhs), Box::new(rhs))),
        Rule::ge => Ok(ExprType::GtEq(Box::new(lhs), Box::new(rhs))),
        Rule::gt => Ok(ExprType::Gt(Box::new(lhs), Box::new(rhs))),
        r => PanicErrorImpl::SyntaxTreeError(format!("unexpected binary operator {:?}", r)).into(),
    }
}

fn prefix_operator(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    let mut children = pair.into_inner();
    let op = children.next().unwrap().as_rule();
    let term = expression(children.next().unwrap())?;
    match op {
        Rule::negate => Ok(ExprType::Negate(Box::new(term))),
        Rule::not => Ok(ExprType::Not(Box::new(term))),
        r => PanicErrorImpl::SyntaxTreeError(format!("unexpected unary prefix operator {:?}", r))
            .into(),
    }
}

fn suffix_operator(pair: Pair<Rule>) -> Result<ExprType, PanicLangError> {
    let mut children = pair.into_inner();
    let term = expression(children.next().unwrap())?;
    let op = children.next().unwrap().as_rule();
    match op {
        Rule::question => Ok(ExprType::Question(Box::new(term))),
        r => PanicErrorImpl::SyntaxTreeError(format!("unexpected unary suffix operator {:?}", r))
            .into(),
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
    let name = Arc::new(String::from(pair.as_span().as_str()));
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
