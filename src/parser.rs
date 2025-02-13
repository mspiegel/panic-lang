use std::iter::Peekable;
use std::str::FromStr;
use std::vec::IntoIter;

use miette::SourceSpan;
use num_bigint::BigInt;

use crate::errors::PanicLangError;
use crate::errors::ParserErrorExpectedToken;
use crate::errors::Result;
use crate::lexer::Token;
use crate::lexer::TokenSpan;

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

fn expected_next(tokens: &mut Peekable<IntoIter<TokenSpan>>) -> Result<TokenSpan> {
    tokens
        .next()
        .ok_or(PanicLangError::ParserErrorUnexpectedEOF)
}

fn expected_token(at: SourceSpan, expected: Token) -> PanicLangError {
    PanicLangError::ParserErrorExpectedToken(ParserErrorExpectedToken {
        at,
        expected: expected.str(),
    })
}

fn to_str(input: &str, at: SourceSpan) -> &str {
    let begin = at.offset();
    let end = at.offset() + at.len();
    &input[begin..end]
}

fn to_string(input: &str, at: SourceSpan) -> String {
    to_str(input, at).to_string()
}

fn parse_definition(input: &str, tokens: &mut Peekable<IntoIter<TokenSpan>>) -> Result<Definition> {
    let mut next = expected_next(tokens)?;
    if !matches!(next.token, Token::LParen) {
        return Err(expected_token(next.span, Token::LParen));
    }
    next = expected_next(tokens)?;
    if !matches!(next.token, Token::Define) {
        return Err(expected_token(next.span, Token::Define));
    }
    next = expected_next(tokens)?;
    if !matches!(next.token, Token::Identifier) {
        return Err(expected_token(next.span, Token::Identifier));
    }
    let identifier = to_string(input, next.span);
    let type_expr = parse_expr(input, tokens)?;
    let body = parse_expr(input, tokens)?;
    Ok(Definition {
        identifier,
        type_expr,
        body,
    })
}

fn parse_expr(input: &str, tokens: &mut Peekable<IntoIter<TokenSpan>>) -> Result<Expr> {
    let mut next = expected_next(tokens)?;
    match next.token {
        Token::Bool(val) => {
            return Ok(Expr::BoolLiteral(val));
        }
        Token::Identifier => {
            let identifier = to_string(input, next.span);
            return Ok(Expr::Identifier(identifier));
        }
        Token::IntLiteral => {
            let text = to_str(input, next.span);
            // TODO: generate parser error for ParseBigIntError
            let num = BigInt::from_str(text).unwrap();
            return Ok(Expr::IntLiteral(num));
        }
        Token::LParen => {
            // consume the left parenthesis
        }
        _ => {
            return Err(PanicLangError::ParserErrorExpectedToken(
                ParserErrorExpectedToken {
                    at: next.span,
                    expected: "<literal>, <identifier>, or '('",
                },
            ))
        }
    }
    next = expected_next(tokens)?;
    match next.token {
        Token::RParen => {
            return Ok(Expr::EmptyList);
        }
        Token::Cond => {}
        Token::Colon => {}
        Token::Lambda => {}
        Token::Question => {
            let expr = parse_expr(input, tokens)?;
            return Ok(Expr::Question(Box::new(expr)));
        }
        Token::RArrow => {}
        _ => {
            // everything else is an application expression
            let function = Box::new(parse_expr(input, tokens)?);
            let mut arguments = vec![];
            return Ok(Expr::Application {
                function,
                arguments,
            });
        }
    }
}

pub fn parse(input: &str, tokens: Vec<TokenSpan>) -> Result<Program> {
    let mut definitions = vec![];
    let mut iter = tokens.into_iter().peekable();
    while iter.peek().is_some() {
        definitions.push(parse_definition(input, &mut iter)?);
    }
    Ok(Program { definitions })
}
