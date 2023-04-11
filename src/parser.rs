use pest::iterators::Pair;
use std::fmt;

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

// Changes to Expr must be propagated to DebugExpr!
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
            Rule::EOI => (),
            _ => panic!("Unexpected rule {:?}", inner.as_rule()),
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
            Rule::stmt_block => body = parse_statement_block(inner),
            Rule::typename => typ = parse_type_name(inner),
            Rule::func_params => params = parse_func_params(inner),
            _ => panic!("Unexpected rule {:?}", inner.as_rule()),
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
    assert_eq!(pair.as_rule(), Rule::func_param);
    let mut name = "".to_string();
    let mut typ = TypeName::Invalid;
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => name = inner.as_str().to_string(),
            Rule::typename => typ = parse_type_name(inner),
            _ => panic!("Unexpected rule {:?}", inner.as_rule()),
        }
    }
    return FuncParam { name, typ };
}

fn parse_func_params(pair: Pair<Rule>) -> Vec<FuncParam> {
    assert_eq!(pair.as_rule(), Rule::func_params);
    let mut params = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::func_param {
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
    assert_eq!(pair.as_rule(), Rule::stmt_block);
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
        Rule::macro_stmt => parse_macro_statement(inner),
        Rule::expr_stmt => parse_expr_statement(inner),
        Rule::return_stmt => parse_return_statement(inner),
        Rule::let_stmt => parse_let_statement(inner),
        Rule::if_stmt => Statement::If(parse_if_statement(inner)),
        _ => panic!("Unexpected rule {:?}", inner.as_rule()),
    }
}

fn parse_macro_statement(pair: Pair<Rule>) -> Statement {
    assert_eq!(pair.as_rule(), Rule::macro_stmt);
    return Statement::Macro(pair.as_str().to_string());
}

fn parse_expr_statement(pair: Pair<Rule>) -> Statement {
    assert_eq!(pair.as_rule(), Rule::expr_stmt);
    return Statement::Expr(parse_expression(pair.into_inner().next().unwrap()));
}

fn parse_return_statement(pair: Pair<Rule>) -> Statement {
    assert_eq!(pair.as_rule(), Rule::return_stmt);
    return Statement::Return(parse_expression(pair.into_inner().next().unwrap()));
}

fn parse_let_statement(pair: Pair<Rule>) -> Statement {
    assert_eq!(pair.as_rule(), Rule::let_stmt);
    let mut name = "".to_string();
    let mut typ = TypeName::Invalid;
    let mut expr = None;
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => name = inner.as_str().to_string(),
            Rule::typename => typ = parse_type_name(inner),
            Rule::expr => expr = Some(parse_expression(inner)),
            _ => panic!("Unexpected rule {:?}", inner.as_rule()),
        }
    }
    let expr = expr.unwrap();
    return Statement::Let(LetStmt { name, typ, expr });
}

fn parse_if_statement(pair: Pair<Rule>) -> IfStmt {
    assert_eq!(pair.as_rule(), Rule::if_stmt);
    let mut conditions = Vec::new();
    let mut statements = Vec::new();
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::expr => conditions.push(parse_expression(inner)),
            Rule::stmt_block => statements.push(parse_statement_block(inner)),
            Rule::if_stmt => {
                let mut nested = parse_if_statement(inner);
                conditions.append(nested.conditions.as_mut());
                statements.append(nested.statements.as_mut());
            }
            _ => panic!("Unexpected rule {:?}", inner.as_rule()),
        }
    }
    return IfStmt {
        conditions,
        statements,
    };
}

fn parse_expression(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::expr);
    let pair = pair.into_inner().next().unwrap(); // Rule::compare
    let mut pair = pair.into_inner();
    let lhs = pair.next();
    let op = pair.next();
    let rhs = pair.next();
    if op.is_none() {
        return parse_addsub(lhs.unwrap());
    }
    let (lhs, op, rhs) = (lhs.unwrap(), op.unwrap(), rhs.unwrap());
    let lhs = Box::new(parse_addsub(lhs));
    let op = op.as_rule();
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

fn parse_addsub(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::addsub);
    let mut pair = pair.into_inner();
    let lhs = pair.next();
    let op = pair.next();
    let rhs = pair.next();
    if op.is_none() {
        return parse_muldiv(lhs.unwrap());
    }
    let (lhs, op, rhs) = (lhs.unwrap(), op.unwrap(), rhs.unwrap());
    let lhs = Box::new(parse_muldiv(lhs));
    let op = op.as_rule();
    let rhs = Box::new(parse_muldiv(rhs));
    let mut result = match op {
        Rule::add => Expr::Add(lhs, rhs),
        Rule::sub => Expr::Sub(lhs, rhs),
        _ => panic!("Unexpected rule {:?}", op),
    };
    let mut op_ = pair.next();
    let mut rhs_ = pair.next();
    while op_.is_some() {
        let (op, rhs) = (op_.unwrap(), rhs_.unwrap());
        let lhs = Box::new(result);
        let op = op.as_rule();
        let rhs = Box::new(parse_muldiv(rhs));
        result = match op {
            Rule::add => Expr::Add(lhs, rhs),
            Rule::sub => Expr::Sub(lhs, rhs),
            _ => panic!("Unexpected rule {:?}", op),
        };
        op_ = pair.next();
        rhs_ = pair.next();
    }
    return result;
}

fn parse_muldiv(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::muldiv);
    let mut pair = pair.into_inner();
    let lhs = pair.next();
    let op = pair.next();
    let rhs = pair.next();
    if op.is_none() {
        return parse_unary(lhs.unwrap());
    }
    let (lhs, op, rhs) = (lhs.unwrap(), op.unwrap(), rhs.unwrap());
    let lhs = Box::new(parse_unary(lhs));
    let op = op.as_rule();
    let rhs = Box::new(parse_unary(rhs));
    let mut result = match op {
        Rule::mul => Expr::Mul(lhs, rhs),
        Rule::div => Expr::Div(lhs, rhs),
        Rule::modulo => Expr::Mod(lhs, rhs),
        _ => panic!("Unexpected rule {:?}", op),
    };
    let mut op_ = pair.next();
    let mut rhs_ = pair.next();
    while op_.is_some() {
        let (op, rhs) = (op_.unwrap(), rhs_.unwrap());
        let lhs = Box::new(result);
        let op = op.as_rule();
        let rhs = Box::new(parse_unary(rhs));
        result = match op {
            Rule::mul => Expr::Mul(lhs, rhs),
            Rule::div => Expr::Div(lhs, rhs),
            Rule::modulo => Expr::Mod(lhs, rhs),
            _ => panic!("Unexpected rule {:?}", op),
        };
        op_ = pair.next();
        rhs_ = pair.next();
    }
    return result;
}

fn parse_unary(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::unary);
    let mut pair = pair.into_inner();
    let left = pair.next().unwrap();
    let right = pair.next();
    if right.is_none() {
        return parse_term(left);
    }
    let left = left.as_rule();
    let right = right.unwrap();
    let right = Box::new(parse_term(right));
    let result = match left {
        Rule::negative => Expr::Neg(right),
        _ => panic!("Unexpected rule {:?}", left),
    };
    return result;
}

fn parse_term(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::term);
    let pair = pair.into_inner().next().unwrap();
    if pair.as_rule() == Rule::func_call {
        return parse_func_call(pair);
    } else {
        return parse_primary(pair);
    }
}

fn parse_func_call(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::func_call);
    let mut pair = pair.into_inner();
    let ident = pair.next().unwrap().as_str().to_string();
    let params = parse_call_params(pair.next());
    return Expr::FuncCall(ident, params);
}

fn parse_call_params(pair: Option<Pair<Rule>>) -> Vec<Expr> {
    let mut params = Vec::new();
    if pair.is_none() {
        return params;
    }
    let pair = pair.unwrap();
    assert_eq!(pair.as_rule(), Rule::call_params);
    let pair = pair.into_inner();
    for inner in pair {
        params.push(parse_expression(inner));
    }
    return params;
}

fn parse_primary(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::primary);
    let pair = pair.into_inner().next().unwrap();
    let rule = pair.as_rule();
    let result = match rule {
        Rule::ident => parse_ident(pair),
        Rule::literal => parse_literal(pair),
        Rule::paren => parse_paren(pair),
        _ => panic!("Unexpected rule {:?}", rule),
    };
    return result;
}

fn parse_ident(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::ident);
    return Expr::Ident(pair.as_str().to_string());
}

fn parse_literal(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::literal);
    let pair = pair.into_inner().next().unwrap();
    let rule = pair.as_rule();
    let result = match rule {
        Rule::int_literal => parse_int_literal(pair),
        Rule::truelit => Expr::Bool(true),
        Rule::falselit => Expr::Bool(false),
        Rule::unitlit => Expr::Unit(),
        _ => panic!("Unexpected rule {:?}", rule),
    };
    return result;
}

fn parse_int_literal(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::int_literal);
    let result = pair.as_str().parse::<i32>().unwrap();
    return Expr::Int32(result);
}

fn parse_paren(pair: Pair<Rule>) -> Expr {
    assert_eq!(pair.as_rule(), Rule::paren);
    let pair = pair.into_inner().next().unwrap();
    return Expr::Paren(Box::new(parse_expression(pair)));
}

#[derive(Debug)]
#[allow(dead_code)]
enum DebugExpr {
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

impl fmt::Debug for Expr {
    // TODO is there a sane way to implement Debug specialization
    // for some of the Expr variants? 
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Int32(i) => write!(f, "Int32({})", i),
            Expr::Bool(b) => write!(f, "Bool({})", b),
            Expr::Ident(s) => write!(f, "Ident({})", s),
            _ => unsafe { (&*(self as *const Expr as *const DebugExpr)).fmt(f) },
        }
    }
}