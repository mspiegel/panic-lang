use thiserror::Error;

use crate::parser::peg_grammar::Rule;

#[derive(Error, Debug)]
pub enum PanicLangError {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("parser error")]
    ParserError(#[from] pest::error::Error<Rule>),
    #[error("syntax tree error")]
    SyntaxTreeError(String),
    #[error("unknown error")]
    Unknown,
}
