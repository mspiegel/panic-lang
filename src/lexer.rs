use logos::Logos;
use miette::SourceSpan;

use crate::errors::to_span;
use crate::errors::LexerError;
use crate::errors::PanicLangError;
use crate::errors::Result;
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

pub struct TokenSpan {
    pub token: Token,
    pub span: SourceSpan,
}

pub fn lex(input: &str) -> Result<Vec<TokenSpan>> {
    let mut tokens = vec![];
    let mut errors = vec![];
    let lexer = Token::lexer(input);
    for (token, range) in lexer.spanned() {
        let span = to_span(&range);
        match token {
            Ok(token) => tokens.push(TokenSpan { token, span }),
            Err(_) => errors.push(PanicLangError::LexerError(LexerError { at: span })),
        }
    }
    if errors.is_empty() {
        Ok(tokens)
    } else if errors.len() == 1 {
        Err(errors.into_iter().next().unwrap().into())
    } else {
        Err(PanicLangError::SeveralErrors(SeveralErrors { errors }))
    }
}

impl Token {
    pub fn str(&self) -> &'static str {
        match self {
            Token::Bool(val) if *val == true => "true",
            Token::Bool(_) => "false",
            Token::Define => "define",
            Token::Lambda => "lambda",
            Token::Cond => "cond",
            Token::Else => "else",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::Question => "?",
            Token::Colon => ":",
            Token::RArrow => "->",
            Token::IntLiteral => "<integer literal>",
            Token::Identifier => "<identifier>",
        }
    }
}
