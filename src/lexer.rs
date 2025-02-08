use logos::Logos;
use miette::Result;
use miette::SourceSpan;

use crate::errors::LexerError;
use crate::errors::PanicLangError;
use crate::errors::SeveralErrors;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
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

pub fn lex(input: &str) -> Result<Vec<Token>> {
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
