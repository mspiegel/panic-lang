use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;
use std::vec::IntoIter;

use miette::SourceSpan;
use num_bigint::BigInt;

use crate::errors::PanicLangError;
use crate::errors::ParserErrorExpectedToken;
use crate::errors::ParserErrorUnexpectedToken;
use crate::errors::Result;
use crate::lexer::Token;
use crate::lexer::TokenSpan;

#[derive(Debug)]
pub struct Program {
    pub definitions: Vec<Definition>,
}

#[derive(Debug)]
pub struct Definition {
    pub identifier: TypedIden,
    pub body: Expr,
}

#[derive(Debug)]
pub struct TypedIden {
    pub identifier: String,
    pub type_expr: Expr,
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

fn expected_peek(tokens: &mut Peekable<IntoIter<TokenSpan>>) -> Result<TokenSpan> {
    tokens
        .peek()
        .cloned()
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
    consume_lparen(tokens)?;
    let identifier = parse_typed_identifier(input, tokens)?;
    let body = parse_expr(input, tokens)?;
    consume_rparen(tokens)?;
    Ok(Definition { identifier, body })
}

fn parse_typed_identifier(
    input: &str,
    tokens: &mut Peekable<IntoIter<TokenSpan>>,
) -> Result<TypedIden> {
    consume_lparen(tokens)?;
    let mut next = expected_next(tokens)?;
    if !matches!(next.token, Token::Colon) {
        return Err(expected_token(next.span, Token::Colon));
    }
    next = expected_next(tokens)?;
    if !matches!(next.token, Token::Identifier) {
        return Err(expected_token(next.span, Token::Identifier));
    }
    let identifier = to_string(input, next.span);
    let type_expr = parse_expr(input, tokens)?;
    consume_rparen(tokens)?;
    Ok(TypedIden {
        identifier,
        type_expr,
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
    next = expected_peek(tokens)?;
    let expr = match next.token {
        Token::RParen => Expr::EmptyList,
        Token::Cond => {
            let mut cond_clauses = vec![];
            _ = expected_next(tokens)?;
            loop {
                consume_lparen(tokens)?;
                if matches!(expected_peek(tokens)?.token, Token::Else) {
                    break;
                }
                let test = parse_expr(input, tokens)?;
                let action = parse_expr(input, tokens)?;
                consume_rparen(tokens)?;
                cond_clauses.push(CondClause { test, action });
            }
            _ = expected_next(tokens)?;
            let else_clause = Box::new(parse_expr(input, tokens)?);
            consume_rparen(tokens)?;
            Expr::Cond {
                cond_clauses,
                else_clause,
            }
        }
        Token::Lambda => {
            let mut formals = vec![];
            _ = expected_next(tokens)?;
            consume_lparen(tokens)?;
            next = expected_next(tokens)?;
            while !matches!(next.token, Token::RParen) {
                formals.push(to_string(input, next.span));
                next = expected_next(tokens)?;
            }
            let body = Box::new(parse_expr(input, tokens)?);
            Expr::Lambda { formals, body }
        }
        Token::Question => {
            _ = expected_next(tokens)?;
            let expr = parse_expr(input, tokens)?;
            Expr::Question(Box::new(expr))
        }
        Token::RArrow => {
            let mut argument_types = vec![];
            _ = expected_next(tokens)?;
            consume_lparen(tokens)?;
            while expected_peek(tokens)?.token != Token::RParen {
                argument_types.push(parse_expr(input, tokens)?);
            }
            consume_rparen(tokens)?;
            let return_type = Box::new(parse_expr(input, tokens)?);
            Expr::RArrow {
                argument_types,
                return_type,
            }
        }
        Token::Identifier | Token::LParen => {
            let function = Box::new(parse_expr(input, tokens)?);
            let mut arguments = vec![];
            while expected_peek(tokens)?.token != Token::RParen {
                arguments.push(parse_expr(input, tokens)?);
            }
            Expr::Application {
                function,
                arguments,
            }
        }
        Token::Colon | Token::Define | Token::Bool(_) | Token::Else | Token::IntLiteral => {
            return Err(PanicLangError::ParserErrorUnexpectedToken(
                ParserErrorUnexpectedToken { at: next.span },
            ));
        }
    };
    consume_rparen(tokens)?;
    Ok(expr)
}

fn consume_lparen(tokens: &mut Peekable<IntoIter<TokenSpan>>) -> Result<()> {
    let next = expected_next(tokens)?;
    if next.token != Token::LParen {
        return Err(expected_token(next.span, Token::LParen));
    }
    Ok(())
}

fn consume_rparen(tokens: &mut Peekable<IntoIter<TokenSpan>>) -> Result<()> {
    let next = expected_next(tokens)?;
    if next.token != Token::RParen {
        return Err(expected_token(next.span, Token::RParen));
    }
    Ok(())
}

pub fn parse(input: &str, tokens: Vec<TokenSpan>) -> Result<Program> {
    let mut definitions = vec![];
    let mut iter = tokens.into_iter().peekable();
    while iter.peek().is_some() {
        definitions.push(parse_definition(input, &mut iter)?);
    }
    Ok(Program { definitions })
}

fn write_slice<T: fmt::Display>(f: &mut fmt::Formatter, v: &[T]) -> fmt::Result {
    for (pos, expr) in v.iter().enumerate() {
        write!(f, "{}", expr)?;
        if pos < (v.len() - 1) {
            write!(f, " ")?;
        }
    }
    Ok(())
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::EmptyList => {
                write!(f, "()")
            }
            Expr::IntLiteral(i) => {
                write!(f, "{}", i)
            }
            Expr::BoolLiteral(true) => {
                write!(f, "true")
            }
            Expr::BoolLiteral(false) => {
                write!(f, "false")
            }
            Expr::Identifier(iden) => {
                write!(f, "{}", iden)
            }
            Expr::Application {
                function,
                arguments,
            } => {
                write!(f, "({}", function)?;
                if !arguments.is_empty() {
                    write!(f, " ")?;
                    write_slice(f, arguments)?;
                }
                write!(f, ")")
            }
            Expr::Cond {
                cond_clauses,
                else_clause,
            } => {
                write!(f, "(cond ")?;
                for cond_clause in cond_clauses {
                    write!(f, "({} {}) ", cond_clause.test, cond_clause.action)?;
                }
                write!(f, "(else {}))", else_clause)
            }
            Expr::Lambda { formals, body } => {
                write!(f, "(lambda (")?;
                write_slice(f, formals)?;
                write!(f, ") {})", body)
            }
            Expr::Question(expr) => {
                write!(f, "(? {})", expr)
            }
            Expr::RArrow {
                argument_types,
                return_type,
            } => {
                write!(f, "(-> (")?;
                write_slice(f, argument_types)?;
                write!(f, ") {})", return_type)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;

    fn parse_input_expr(input: &str) -> Result<Expr> {
        let tokens = lex(input)?;
        let mut iter = tokens.into_iter().peekable();
        parse_expr(input, &mut iter)
    }

    fn test_roundtrip_expr(input: &str) -> Result<()> {
        let expr = parse_input_expr(input)?;
        assert_eq!(format!("{}", expr), input);
        Ok(())
    }

    #[test]
    fn test_parse_expr() -> Result<()> {
        test_roundtrip_expr("()")?;
        test_roundtrip_expr("0")?;
        test_roundtrip_expr("i32")?;
        test_roundtrip_expr("true")?;
        test_roundtrip_expr("false")?;
        test_roundtrip_expr("(foo)")?;
        test_roundtrip_expr("(foo 1 2 3)")?;
        test_roundtrip_expr("(-> () ())")?;
        test_roundtrip_expr("(-> (i32) i32)")?;
        test_roundtrip_expr("(cond (else ()))")?;
        test_roundtrip_expr("(cond ((< a 0) true) ((== a 0) false) (else ()))")?;
        Ok(())
    }
}
