use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;
use std::vec::IntoIter;

use miette::SourceSpan;
use num_bigint::BigInt;

use crate::errors::PanicLangError;
use crate::errors::ParserErrorExpectedExpr;
use crate::errors::ParserErrorExpectedToken;
use crate::errors::ParserErrorNotAFunctionApplication;
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

    StringLiteral(String),

    Identifier(String),

    Application {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },

    Cond {
        cond_clauses: Vec<CondClause>,
        else_clause: Box<Expr>,
    },

    And {
        exprs: Vec<Expr>,
    },

    Or {
        exprs: Vec<Expr>,
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
    consume_token(tokens, Token::Define)?;
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
    consume_token(tokens, Token::Colon)?;
    let next = expected_next(tokens)?;
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
        Token::StrLiteral => {
            // TODO: parse escape characters
            let begin = next.span.offset();
            let end = begin + next.span.len();
            let literal = input[(begin + 1)..(end - 1)].to_string();
            return Ok(Expr::StringLiteral(literal));
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
        Token::And => {
            let mut exprs = vec![];
            consume_token(tokens, Token::And)?;
            while expected_peek(tokens)?.token != Token::RParen {
                exprs.push(parse_expr(input, tokens)?);
            }
            if exprs.is_empty() {
                return Err(PanicLangError::ParserErrorExpectedExpr(
                    ParserErrorExpectedExpr {
                        at: expected_peek(tokens)?.span,
                    },
                ));
            }
            Expr::And { exprs }
        }
        Token::Or => {
            let mut exprs = vec![];
            consume_token(tokens, Token::Or)?;
            while expected_peek(tokens)?.token != Token::RParen {
                exprs.push(parse_expr(input, tokens)?);
            }
            if exprs.is_empty() {
                return Err(PanicLangError::ParserErrorExpectedExpr(
                    ParserErrorExpectedExpr {
                        at: expected_peek(tokens)?.span,
                    },
                ));
            }
            Expr::Or { exprs }
        }
        Token::Cond => {
            let mut cond_clauses = vec![];
            consume_token(tokens, Token::Cond)?;
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
            consume_token(tokens, Token::Else)?;
            let else_clause = Box::new(parse_expr(input, tokens)?);
            consume_rparen(tokens)?;
            Expr::Cond {
                cond_clauses,
                else_clause,
            }
        }
        Token::Lambda => {
            let mut formals = vec![];
            consume_token(tokens, Token::Lambda)?;
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
            consume_token(tokens, Token::Question)?;
            let expr = parse_expr(input, tokens)?;
            Expr::Question(Box::new(expr))
        }
        Token::RArrow => {
            let mut argument_types = vec![];
            consume_token(tokens, Token::RArrow)?;
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
        Token::Colon
        | Token::Define
        | Token::Bool(_)
        | Token::Else
        | Token::IntLiteral
        | Token::StrLiteral => {
            return Err(PanicLangError::ParserErrorNotAFunctionApplication(
                ParserErrorNotAFunctionApplication { at: next.span },
            ));
        }
    };
    consume_rparen(tokens)?;
    Ok(expr)
}

fn consume_token(tokens: &mut Peekable<IntoIter<TokenSpan>>, expected: Token) -> Result<()> {
    let next = expected_next(tokens)?;
    if next.token != expected {
        return Err(expected_token(next.span, expected));
    }
    Ok(())
}

fn consume_lparen(tokens: &mut Peekable<IntoIter<TokenSpan>>) -> Result<()> {
    consume_token(tokens, Token::LParen)
}

fn consume_rparen(tokens: &mut Peekable<IntoIter<TokenSpan>>) -> Result<()> {
    consume_token(tokens, Token::RParen)
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
            Expr::StringLiteral(lit) => {
                write!(f, "\"{}\"", lit)
            }
            Expr::And { exprs } => {
                write!(f, "(and ")?;
                write_slice(f, exprs)?;
                write!(f, ")")
            }
            Expr::Or { exprs } => {
                write!(f, "(or ")?;
                write_slice(f, exprs)?;
                write!(f, ")")
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

impl fmt::Display for TypedIden {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(: {} {})", self.identifier, self.type_expr)
    }
}

impl fmt::Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(define {} {})", self.identifier, self.body)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_slice(f, &self.definitions)
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

    fn parse_input_program(input: &str) -> Result<Program> {
        let tokens = lex(input)?;
        parse(input, tokens)
    }

    fn test_roundtrip_expr(input: &str) -> Result<()> {
        let expr = parse_input_expr(input)?;
        assert_eq!(format!("{}", expr), input);
        Ok(())
    }

    fn test_roundtrip_program(input: &str) -> Result<()> {
        let program = parse_input_program(input)?;
        assert_eq!(format!("{}", program), input);
        Ok(())
    }

    #[test]
    fn test_parse_expr() -> Result<()> {
        test_roundtrip_expr("()")?;
        test_roundtrip_expr("0")?;
        test_roundtrip_expr("\"foo\"")?;
        test_roundtrip_expr("i32")?;
        test_roundtrip_expr("true")?;
        test_roundtrip_expr("false")?;
        test_roundtrip_expr("(foo)")?;
        test_roundtrip_expr("(foo 1 2 3)")?;
        test_roundtrip_expr("(and true true)")?;
        test_roundtrip_expr("(or true false)")?;
        test_roundtrip_expr("(lambda (x) x)")?;
        test_roundtrip_expr("(? (/ 1 0))")?;
        test_roundtrip_expr("(-> () ())")?;
        test_roundtrip_expr("(-> (i32) i32)")?;
        test_roundtrip_expr("(cond (else ()))")?;
        test_roundtrip_expr("(cond ((< a 0) true) ((== a 0) false) (else ()))")?;
        Ok(())
    }

    #[test]
    fn test_parse_expr_errors() -> Result<()> {
        assert!(matches!(
            parse_input_expr("(true true)"),
            Err(PanicLangError::ParserErrorNotAFunctionApplication(_))
        ));
        assert!(matches!(
            parse_input_expr("(foo 1 2"),
            Err(PanicLangError::ParserErrorUnexpectedEOF)
        ));
        assert!(matches!(
            parse_input_expr("(and)"),
            Err(PanicLangError::ParserErrorExpectedExpr(_))
        ));
        assert!(matches!(
            parse_input_expr("(cond)"),
            Err(PanicLangError::ParserErrorExpectedToken(_))
        ));
        Ok(())
    }

    #[test]
    fn test_parse_program() -> Result<()> {
        test_roundtrip_program("(define (: foo i32) 0)")?;
        test_roundtrip_program("(define (: main (-> () ())) (lambda () ()))")?;
        Ok(())
    }
}
