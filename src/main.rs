extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate pest_ascii_tree;

use std::io;
use std::io::BufRead;

use pest::iterators::Pair;
use pest::Parser;
use pest_ascii_tree::print_ascii_tree;

#[derive(Parser)]
#[grammar = "panic.pest"]
struct PanicParser;

#[derive(Debug)]
struct Program {
    decls: Vec<TopDecl>,
}

#[derive(Debug)]
enum TopDecl {
    Func(Function),
}

#[derive(Debug)]
struct Function {
    name: String,
    params: Vec<FuncParam>,
    typ: TypeName,
    body: Statements,
}

#[derive(Debug)]
struct FuncParam {
    name: String,
    typ: TypeName,
}

#[derive(Debug)]
enum TypeName {
    Invalid,
    Int32,
    Bool,
    Unit,
}

#[derive(Debug)]
enum Statement {
    Expr(Expr),
    Return(Expr),
    Macro(String),
    Let(LetStmt),
    If(IfStmt),
}

#[derive(Debug)]
struct LetStmt {
    name: String,
    typ: TypeName,
    expr: Expr,
}

#[derive(Debug)]
struct IfStmt {
    conditions: Vec<Expr>,
    statements: Vec<Statements>,
}

type Statements = Vec<Statement>;

#[derive(Debug)]
enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Equ(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
    FuncCall(String, Vec<Expr>),
    Ident(String),
    Bool(bool),
    Int32(i32),
    Unit(),
}

fn parse_program(pair: Pair<Rule>) -> Program {
    assert_eq!(pair.as_rule(), Rule::program);
    let mut decls = Vec::new();
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::topdecl => decls.push(parse_top_decl(inner)),
            _ => (),
        }
    }
    return Program { decls };
}

fn parse_top_decl(pair: Pair<Rule>) -> TopDecl {
    assert_eq!(pair.as_rule(), Rule::topdecl);
    return TopDecl::Func(parse_function(pair.into_inner().next().unwrap()));
}

fn parse_function(pair: Pair<Rule>) -> Function {
    assert_eq!(pair.as_rule(), Rule::function);
    let mut name = "".to_string();
    let mut params = Vec::new();
    let mut typ = TypeName::Invalid;
    let mut body = Vec::new();
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => name = inner.as_str().to_string(),
            Rule::stmtBlock => body = parse_statement_block(inner),
            Rule::typename => typ = parse_type_name(inner),
            Rule::funcParams => params = parse_func_params(inner),
            _ => (),
        };
    }
    return Function {
        name,
        params,
        typ,
        body,
    };
}

fn parse_func_param(pair: Pair<Rule>) -> FuncParam {
    assert_eq!(pair.as_rule(), Rule::funcParam);
    let mut name = "".to_string();
    let mut typ = TypeName::Invalid;
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => name = inner.as_str().to_string(),
            Rule::typename => typ = parse_type_name(inner),
            _ => (),
        }
    }
    return FuncParam { name, typ };
}


fn parse_func_params(pair: Pair<Rule>) -> Vec<FuncParam> {
    assert_eq!(pair.as_rule(), Rule::funcParams);
    let mut params = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::funcParam {
            params.push(parse_func_param(inner));
        }
    }
    return params;
}

fn parse_type_name(pair: Pair<Rule>) -> TypeName {
    assert_eq!(pair.as_rule(), Rule::typename);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::typei32 => TypeName::Int32,
        Rule::typebool => TypeName::Bool,
        Rule::typeunit => TypeName::Unit,
        _ => TypeName::Invalid,
    }
}

fn parse_statement_block(pair: Pair<Rule>) -> Statements {
    assert_eq!(pair.as_rule(), Rule::stmtBlock);
    let mut stmts = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::stmt {
            stmts.push(parse_statement(inner));
        }
    }
    return stmts;
}

fn parse_statement(pair: Pair<Rule>) -> Statement {
    assert_eq!(pair.as_rule(), Rule::stmt);
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::macroStmt => parse_macro_statement(inner),
        Rule::exprStmt => parse_expr_statement(inner),
        _ => todo!("statement type not implemented"),
    }
}

fn parse_macro_statement(pair: Pair<Rule>) -> Statement {
    assert_eq!(pair.as_rule(), Rule::macroStmt);
    return Statement::Macro(pair.as_str().to_string());
}

fn parse_expr_statement(pair: Pair<Rule>) -> Statement {
    assert_eq!(pair.as_rule(), Rule::exprStmt);
    return Statement::Expr(
        parse_expression(
            pair.into_inner().next().unwrap(),
        )
    );
}

fn parse_expression(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::expr);
    let pair = pair.into_inner().next().unwrap(); // Rule::compare
    let mut pair:Vec<_> = pair.into_inner().collect();
    if pair.len() == 1 {
        return parse_addsub(pair.pop().unwrap());
    }
    if pair.len() == 3 {
        let (rhs, op, lhs) = (pair.pop(), pair.pop(), pair.pop());
        let (rhs, op, lhs) = (rhs.unwrap(), op.unwrap(), lhs.unwrap());
        let lhs = Box::new(parse_addsub(lhs));
        let op  = op.as_rule();
        let rhs = Box::new(parse_addsub(rhs));
        let result = match op {
            Rule::lt => Expr::Lt(lhs, rhs),
            Rule::le => Expr::Le(lhs, rhs),
            Rule::eq => Expr::Equ(lhs, rhs),
            Rule::ne => Expr::Neq(lhs, rhs),
            Rule::ge => Expr::Ge(lhs, rhs),
            Rule::gt => Expr::Gt(lhs, rhs),
            _ => panic!("Unexpected rule {:?}", op),
        };
        return result;
    }
    panic!("Unexpected expression length {}", pair.len());
}

fn parse_addsub(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::addsub);
    todo!("yolo");
}

fn main() {
    let stdin = io::stdin();
    let lines: Vec<_> = stdin.lock().lines().collect::<Result<_, _>>().unwrap();
    let input = lines.join("\n");

    print_ascii_tree(PanicParser::parse(Rule::program, &input));

    let pairs = PanicParser::parse(Rule::program, &input);
    if pairs.is_err() {
        panic!("{}", pairs.unwrap_err());
    }
    let prog = parse_program(pairs.unwrap().next().unwrap());
    println!("{:?}", prog);
}
