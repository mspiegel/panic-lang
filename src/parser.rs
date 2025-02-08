use num_bigint::BigInt;

use crate::lexer::Token;

#[derive(Debug)]
pub struct Program {
    pub definitions: Vec<Definition>,
}

#[derive(Debug)]
pub struct Definition {
    pub identifier: String,
    pub type_expr: Expr,
    pub body: Expr,
}

#[derive(Debug)]
pub enum Expr {
    EmptyList,

    IntLiteral(BigInt),

    BoolLiteral(bool),

    Identifier(String),

    Application {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },

    Cond {
        cond_clauses: Vec<CondClause>,
        else_clause: Box<Expr>,
    },

    Lambda {
        formals: Vec<String>,
        body: Box<Expr>,
    },

    Question(Box<Expr>),

    RArrow {
        argument_types: Vec<Expr>,
        return_type: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct CondClause {
    pub test: Expr,
    pub action: Expr,
}

pub fn parse(tokens: Vec<Token>) -> Program {
    Program {
        definitions: vec![],
    }
}
