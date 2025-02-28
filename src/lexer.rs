use logos::Logos;
use miette::SourceSpan;

use crate::errors::to_span;
use crate::errors::LexerError;
use crate::errors::PanicLangError;
use crate::errors::Result;
use crate::errors::SeveralErrors;

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),

    #[token("define")]
    Define,

    #[token("lambda")]
    Lambda,

    #[token("let*")]
    LetStar,

    #[token("begin")]
    Begin,

    #[token("cond")]
    Cond,

    #[token("else")]
    Else,

    #[token("and")]
    And,

    #[token("if")]
    If,

    #[token("or")]
    Or,

    #[token("when")]
    When,

    #[token("unless")]
    Unless,

    #[token("set!")]
    SetBang,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[regex(r"-?\d[_\d]*", priority = 2)]
    IntLiteral,

    #[regex(r#"'(?:[^'\r\n\t\\]|\\r|\\n|\\t|\\\\|\\0|\\'|\\")'"#, priority = 2)]
    CharLiteral,

    #[regex(r#""(?:[^"\\]|\\[\s\S])*""#, priority = 2)]
    StrLiteral,

    #[regex(r"[^\s()]+", priority = 1)]
    Name,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TokenSpan {
    pub token: Token,
    pub span: SourceSpan,
}

pub fn span(begin: SourceSpan, end: SourceSpan) -> SourceSpan {
    let start = begin.offset();
    let last = end.offset() + end.len();
    let length = last - start;
    SourceSpan::new(start.into(), length)
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
        Err(errors.into_iter().next().unwrap())
    } else {
        Err(PanicLangError::SeveralErrors(SeveralErrors { errors }))
    }
}

impl Token {
    pub fn str(&self) -> &'static str {
        match self {
            Token::Bool(val) => {
                if *val {
                    "true"
                } else {
                    "false"
                }
            }
            Token::Define => "define",
            Token::Lambda => "lambda",
            Token::LetStar => "let*",
            Token::Begin => "begin",
            Token::Cond => "cond",
            Token::Else => "else",
            Token::And => "and",
            Token::Or => "or",
            Token::If => "if",
            Token::When => "when",
            Token::Unless => "unless",
            Token::SetBang => "set!",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::IntLiteral => "<integer literal>",
            Token::StrLiteral => "<string literal>",
            Token::CharLiteral => "<character literal>",
            Token::Name => "<name>",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() -> Result<()> {
        let tokens =
            lex("true false define lambda begin let* set! cond else if when unless and or ( ) 0 foo \"bar\" \'\\0\'")?;
        let strs = tokens
            .iter()
            .map(|x| x.token.str())
            .collect::<Vec<&str>>()
            .join(" ");
        assert_eq!(
            strs,
            "true false define lambda begin let* set! cond else if when unless and or ( ) <integer literal> <name> <string literal> <character literal>"
        );
        Ok(())
    }

    #[test]
    fn test_flexible_names() -> Result<()> {
        let tokens = lex("empty?")?;
        assert_eq!(
            vec![TokenSpan {
                token: Token::Name,
                span: SourceSpan::new(0.into(), 6),
            }],
            tokens
        );
        let tokens = lex("🤔")?;
        assert_eq!(
            vec![TokenSpan {
                token: Token::Name,
                span: SourceSpan::new(0.into(), 4),
            }],
            tokens
        );
        let tokens = lex("[]")?;
        assert_eq!(
            vec![TokenSpan {
                token: Token::Name,
                span: SourceSpan::new(0.into(), 2),
            }],
            tokens
        );
        let tokens = lex(".")?;
        assert_eq!(
            vec![TokenSpan {
                token: Token::Name,
                span: SourceSpan::new(0.into(), 1),
            }],
            tokens
        );
        Ok(())
    }
}
