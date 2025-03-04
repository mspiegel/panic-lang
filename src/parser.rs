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
use crate::lexer::span;
use crate::lexer::Token;
use crate::lexer::TokenSpan;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub struct Program {
    pub definitions: Vec<Definition>,
}

#[derive(Debug)]
pub struct Definition {
    pub identifier: Identifier,
    pub body: Expr,
    pub at: SourceSpan,
}

#[derive(Debug)]
pub struct Expr {
    pub contents: ExprContents,
    pub at: SourceSpan,
}

#[derive(Debug)]
pub enum ExprContents {
    EmptyList,

    IntLiteral(BigInt),

    BoolLiteral(bool),

    CharLiteral(char),

    StringLiteral(String),

    Reference(Identifier),

    Application {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },

    Cond {
        cond_clauses: Vec<CondClause>,
        else_clause: Box<Expr>,
    },

    Begin {
        exprs: Vec<Expr>,
    },

    LetStar {
        bindings: Vec<BindingClause>,
        body: Box<Expr>,
    },

    SetBang {
        lvalue: Box<Expr>,
        rvalue: Box<Expr>,
    },

    When {
        test: Box<Expr>,
        action: Box<Expr>,
    },

    Unless {
        test: Box<Expr>,
        action: Box<Expr>,
    },

    If {
        test_expr: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },

    And {
        exprs: Vec<Expr>,
    },

    Or {
        exprs: Vec<Expr>,
    },

    Lambda {
        formals: Vec<Identifier>,
        body: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct CondClause {
    pub test: Expr,
    pub action: Expr,
}

#[derive(Debug)]
pub struct BindingClause {
    pub identifier: Identifier,
    pub body: Expr,
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

fn parse_string_literal(input: &str, span: &SourceSpan) -> Result<String> {
    // TODO: parse escape characters
    let begin = span.offset();
    let end = begin + span.len();
    Ok(input[(begin + 1)..(end - 1)].to_string())
}

fn parse_identifier(input: &str, tokens: &mut Peekable<IntoIter<TokenSpan>>) -> Result<Identifier> {
    let next = expected_next(tokens)?;
    if !matches!(next.token, Token::Name) {
        return Err(expected_token(next.span, Token::Name));
    }
    Ok(Identifier(to_string(input, next.span)))
}

fn parse_definition(input: &str, tokens: &mut Peekable<IntoIter<TokenSpan>>) -> Result<Definition> {
    let begin = expected_peek(tokens)?.span;
    consume_lparen(tokens)?;
    consume_token(tokens, Token::Define)?;
    let identifier = parse_identifier(input, tokens)?;
    let body = parse_expr(input, tokens)?;
    let end = expected_peek(tokens)?.span;
    let at = span(begin, end);
    consume_rparen(tokens)?;
    Ok(Definition {
        identifier,
        body,
        at,
    })
}

pub fn parse_expr(input: &str, tokens: &mut Peekable<IntoIter<TokenSpan>>) -> Result<Expr> {
    let begin = expected_peek(tokens)?;
    let begin = begin.span;
    let (contents, end) = parse_expr_contents(input, tokens)?;
    Ok(Expr {
        contents,
        at: span(begin, end),
    })
}

fn parse_expr_contents(
    input: &str,
    tokens: &mut Peekable<IntoIter<TokenSpan>>,
) -> Result<(ExprContents, SourceSpan)> {
    let next = expected_next(tokens)?;
    let contents = match next.token {
        Token::Bool(val) => Some(ExprContents::BoolLiteral(val)),
        Token::Name => {
            let identifier = Identifier(to_string(input, next.span));
            Some(ExprContents::Reference(identifier))
        }
        Token::CharLiteral => {
            let begin = next.span.offset();
            let end = begin + next.span.len();
            let literal = &input[(begin + 1)..(end - 1)];
            match literal {
                "\\n" => Some(ExprContents::CharLiteral('\n')),
                "\\t" => Some(ExprContents::CharLiteral('\t')),
                "\\r" => Some(ExprContents::CharLiteral('\r')),
                "\\0" => Some(ExprContents::CharLiteral('\0')),
                "\\'" => Some(ExprContents::CharLiteral('\'')),
                "\\\"" => Some(ExprContents::CharLiteral('\"')),
                "\\\\" => Some(ExprContents::CharLiteral('\\')),
                _ => {
                    if literal.chars().count() != 1 {
                        return Err(PanicLangError::LexerError(crate::errors::LexerError {
                            at: next.span,
                        }));
                    }
                    Some(ExprContents::CharLiteral(literal.chars().next().unwrap()))
                }
            }
        }
        Token::StrLiteral => {
            let literal = parse_string_literal(input, &next.span)?;
            Some(ExprContents::StringLiteral(literal))
        }
        Token::IntLiteral => {
            let text = to_str(input, next.span);
            // TODO: generate parser error for ParseBigIntError
            let num = BigInt::from_str(text).unwrap();
            Some(ExprContents::IntLiteral(num))
        }
        Token::LParen => {
            // consume the left parenthesis
            None
        }
        _ => {
            return Err(PanicLangError::ParserErrorExpectedToken(
                ParserErrorExpectedToken {
                    at: next.span,
                    expected: "<literal>, <name>, or '('",
                },
            ))
        }
    };
    if let Some(contents) = contents {
        return Ok((contents, next.span));
    }
    let next = expected_peek(tokens)?;
    let contents = match next.token {
        Token::RParen => ExprContents::EmptyList,
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
            ExprContents::And { exprs }
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
            ExprContents::Or { exprs }
        }
        Token::If => {
            consume_token(tokens, Token::If)?;
            let test_expr = Box::new(parse_expr(input, tokens)?);
            let then_expr = Box::new(parse_expr(input, tokens)?);
            let else_expr = Box::new(parse_expr(input, tokens)?);
            ExprContents::If {
                test_expr,
                then_expr,
                else_expr,
            }
        }
        Token::When => {
            consume_token(tokens, Token::When)?;
            let test = Box::new(parse_expr(input, tokens)?);
            let action = Box::new(parse_expr(input, tokens)?);
            ExprContents::When { test, action }
        }
        Token::Unless => {
            consume_token(tokens, Token::Unless)?;
            let test = Box::new(parse_expr(input, tokens)?);
            let action = Box::new(parse_expr(input, tokens)?);
            ExprContents::Unless { test, action }
        }
        Token::Begin => {
            consume_token(tokens, Token::Begin)?;
            let mut exprs = vec![];
            while expected_peek(tokens)?.token != Token::RParen {
                exprs.push(parse_expr(input, tokens)?);
            }
            ExprContents::Begin { exprs }
        }
        Token::LetStar => {
            let mut bindings = vec![];
            consume_token(tokens, Token::LetStar)?;
            consume_lparen(tokens)?;
            while expected_peek(tokens)?.token != Token::RParen {
                consume_lparen(tokens)?;
                let identifier = parse_identifier(input, tokens)?;
                let body = parse_expr(input, tokens)?;
                consume_rparen(tokens)?;
                bindings.push(BindingClause { identifier, body });
            }
            consume_rparen(tokens)?;
            let body = Box::new(parse_expr(input, tokens)?);
            ExprContents::LetStar { bindings, body }
        }
        Token::SetBang => {
            consume_token(tokens, Token::SetBang)?;
            let lvalue = Box::new(parse_expr(input, tokens)?);
            let rvalue = Box::new(parse_expr(input, tokens)?);
            ExprContents::SetBang { lvalue, rvalue }
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
            ExprContents::Cond {
                cond_clauses,
                else_clause,
            }
        }
        Token::Lambda => {
            let mut formals = vec![];
            consume_token(tokens, Token::Lambda)?;
            consume_lparen(tokens)?;
            while expected_peek(tokens)?.token != Token::RParen {
                // TODO: error on duplicate formal parameter
                let formal = parse_identifier(input, tokens)?;
                formals.push(formal);
            }
            consume_rparen(tokens)?;
            let body = Box::new(parse_expr(input, tokens)?);
            ExprContents::Lambda { formals, body }
        }
        Token::Name | Token::LParen => {
            let function = Box::new(parse_expr(input, tokens)?);
            let mut arguments = vec![];
            while expected_peek(tokens)?.token != Token::RParen {
                arguments.push(parse_expr(input, tokens)?);
            }
            ExprContents::Application {
                function,
                arguments,
            }
        }
        Token::Define
        | Token::Bool(_)
        | Token::Else
        | Token::IntLiteral
        | Token::CharLiteral
        | Token::StrLiteral => {
            return Err(PanicLangError::ParserErrorNotAFunctionApplication(
                ParserErrorNotAFunctionApplication { at: next.span },
            ));
        }
    };
    let next = expected_next(tokens)?;
    if !matches!(next.token, Token::RParen) {
        return Err(expected_token(next.span, Token::RParen));
    }
    Ok((contents, next.span))
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
        match &self.contents {
            ExprContents::EmptyList => {
                write!(f, "()")
            }
            ExprContents::IntLiteral(i) => {
                write!(f, "{}", i)
            }
            ExprContents::BoolLiteral(true) => {
                write!(f, "true")
            }
            ExprContents::BoolLiteral(false) => {
                write!(f, "false")
            }
            ExprContents::Reference(reference) => {
                write!(f, "{}", reference)
            }
            ExprContents::CharLiteral(lit) => {
                write!(f, "{:?}", lit)
            }
            ExprContents::StringLiteral(lit) => {
                write!(f, "{:?}", lit)
            }
            ExprContents::Begin { exprs } => {
                write!(f, "(begin")?;
                if !exprs.is_empty() {
                    write!(f, " ")?;
                    write_slice(f, exprs)?;
                }
                write!(f, ")")
            }
            ExprContents::And { exprs } => {
                write!(f, "(and ")?;
                write_slice(f, exprs)?;
                write!(f, ")")
            }
            ExprContents::Or { exprs } => {
                write!(f, "(or ")?;
                write_slice(f, exprs)?;
                write!(f, ")")
            }
            ExprContents::If {
                test_expr,
                then_expr,
                else_expr,
            } => {
                write!(f, "(if {} {} {})", test_expr, then_expr, else_expr)
            }
            ExprContents::When { test, action } => {
                write!(f, "(when {} {})", test, action)
            }
            ExprContents::Unless { test, action } => {
                write!(f, "(unless {} {})", test, action)
            }
            ExprContents::Application {
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
            ExprContents::Cond {
                cond_clauses,
                else_clause,
            } => {
                write!(f, "(cond ")?;
                for cond_clause in cond_clauses {
                    write!(f, "({} {}) ", cond_clause.test, cond_clause.action)?;
                }
                write!(f, "(else {}))", else_clause)
            }
            ExprContents::LetStar { bindings, body } => {
                write!(f, "(let* (")?;
                write_slice(f, bindings)?;
                write!(f, ") {})", body)
            }
            ExprContents::Lambda { formals, body } => {
                write!(f, "(lambda (")?;
                write_slice(f, formals)?;
                write!(f, ") {})", body)
            }
            ExprContents::SetBang { lvalue, rvalue } => {
                write!(f, "(set! {} {})", lvalue, rvalue)
            }
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl fmt::Display for BindingClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {})", self.identifier, self.body)
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
        test_roundtrip_expr("'\\n'")?;
        test_roundtrip_expr("'\\t'")?;
        test_roundtrip_expr("'\\r'")?;
        test_roundtrip_expr("'\\0'")?;
        test_roundtrip_expr("'\\\\'")?;
        test_roundtrip_expr("'\\\''")?;
        test_roundtrip_expr("'\\\"")?;
        test_roundtrip_expr("'0'")?;
        test_roundtrip_expr("\"foo\"")?;
        test_roundtrip_expr("i32")?;
        test_roundtrip_expr("true")?;
        test_roundtrip_expr("false")?;
        test_roundtrip_expr("foo")?;
        test_roundtrip_expr("(foo)")?;
        test_roundtrip_expr("(foo 1 2 3)")?;
        test_roundtrip_expr("(. foo \"x\")")?; // get field x of foo
        test_roundtrip_expr("(set! (. foo \"x\") 0)")?; // set field x of foo
        test_roundtrip_expr("(and true true)")?;
        test_roundtrip_expr("(or true false)")?;
        test_roundtrip_expr("(if a b c)")?;
        test_roundtrip_expr("(when true ())")?;
        test_roundtrip_expr("(unless true ())")?;
        test_roundtrip_expr("(lambda (x) x)")?;
        test_roundtrip_expr("(begin)")?;
        test_roundtrip_expr("(begin (foo 1) 2 3)")?;
        test_roundtrip_expr("(let* () true)")?;
        test_roundtrip_expr("(let* ((a 1) (b 2)) (+ a b))")?;
        test_roundtrip_expr("(? (/ 1 0))")?;
        test_roundtrip_expr("(-> () ())")?;
        test_roundtrip_expr("(-> (i32) i32)")?;
        test_roundtrip_expr("([] i32)")?; // slice of i32
        test_roundtrip_expr("([] foo 0)")?; // get element 0 of foo
        test_roundtrip_expr("(set! ([] foo 0) \"hello\")")?; // set element 0 of foo
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
        test_roundtrip_program("(define foo 0)")?;
        test_roundtrip_program("(define main (lambda () ()))")?;
        Ok(())
    }
}
