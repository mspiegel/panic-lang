use std::io;

use logos::Logos;
use miette::Diagnostic;
use miette::IntoDiagnostic;
use miette::Result;
use miette::SourceSpan;
use num_bigint::BigInt;
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum PanicLangError {
    #[error(transparent)]
    #[diagnostic(code(panic_lang::io_error))]
    IoError(#[from] std::io::Error),

    #[error(transparent)]
    #[diagnostic(transparent)]
    LexerError(#[from] LexerError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    SeveralErrors(#[from] SeveralErrors),
}

#[derive(Error, Diagnostic, Debug)]
#[error("lexer error")]
pub struct LexerError {
    #[label("here")]
    pub at: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("several errors")]
pub struct SeveralErrors {
    #[related]
    pub errors: Vec<PanicLangError>,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),

    #[token("define")]
    Define,

    #[token("lambda")]
    Lambda,

    #[token("cond")]
    Cond,

    #[token("else")]
    Else,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("?", priority = 2)]
    Question,

    #[token(":")]
    Colon,

    #[token("->")]
    RArrow,

    #[regex(r"-?\d[_\d]*", priority = 2)]
    IntLiteral,

    #[regex(r"[^\s()]+", priority = 1)]
    Identifier,
}

#[derive(Debug)]
struct Program {
    definitions: Vec<Definition>,
}

#[derive(Debug)]
struct Definition {
    identifier: String,
    type_expr: Expr,
    body: Expr,
}

#[derive(Debug)]
enum Expr {
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
struct CondClause {
    test: Expr,
    action: Expr,
}

fn lex(input: &str) -> Result<Vec<Token>> {
    let mut tokens = vec![];
    let mut errors = vec![];
    let lexer = Token::lexer(input);
    for (token, span) in lexer.spanned() {
        match token {
            Ok(token) => tokens.push(token),
            Err(_) => errors.push(PanicLangError::LexerError(LexerError {
                at: SourceSpan::new(span.start.into(), span.end - span.start),
            })),
        }
    }
    if errors.len() > 0 {
        Err(PanicLangError::SeveralErrors(SeveralErrors { errors }).into())
    } else {
        Ok(tokens)
    }
}

fn parse(tokens: Vec<Token>) -> Program {
    Program {
        definitions: vec![],
    }
}

fn main_helper(input: &str) -> Result<()> {
    let tokens = lex(input)?;
    println!("{:?}", tokens);
    let program = parse(tokens);
    println!("{:?}", program);
    Ok(())
}

fn main() -> Result<()> {
    let input = io::read_to_string(io::stdin()).into_diagnostic()?;
    main_helper(&input).map_err(|error| error.with_source_code(input))
}
