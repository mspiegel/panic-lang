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

    #[token("fn")]
    Fn,

    #[token("struct")]
    Struct,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("new")]
    New,

    #[token("return")]
    Return,

    #[token("mut")]
    Mut,

    #[token("inout")]
    Inout,

    #[token("()")]
    Void,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Asterisk,

    #[token("/")]
    ForwardSlash,

    #[token("{")]
    LBracket,

    #[token("}")]
    RBracket,

    #[token("->")]
    RArrow,

    #[token(":")]
    Colon,

    #[token(";")]
    Semicolon,

    #[token(",")]
    Comma,

    #[token("|")]
    Pipe,

    #[regex(r"-?\d[_\d]*", priority = 2)]
    IntLiteral,

    #[regex(r#"'(?:[^'\r\n\t\\]|\\r|\\n|\\t|\\\\|\\0|\\'|\\")'"#, priority = 2)]
    CharLiteral,

    #[regex(r#""(?:[^"\\]|\\[\s\S])*""#, priority = 2)]
    StrLiteral,

    #[regex(r"[A-Za-z_][[:word:]]*", priority = 1)]
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
            Token::Fn => "fn",
            Token::Struct => "struct",
            Token::If => "if",
            Token::Else => "else",
            Token::New => "new",
            Token::Return => "return",
            Token::Mut => "mut",
            Token::Inout => "inout",
            Token::Void => "()",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Asterisk => "*",
            Token::ForwardSlash => "/",
            Token::LBracket => "{",
            Token::RBracket => "}",
            Token::RArrow => "->",
            Token::Colon => ":",
            Token::Semicolon => ";",
            Token::Comma => ",",
            Token::Pipe => "|",
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
            lex("true false define fn struct if else new return mut inout () ( ) + - * / { } -> : ; , | 0 -0 foo foo9 _ _foo \"bar\" \'\\0\'")?;
        let strs = tokens
            .iter()
            .map(|x| x.token.str())
            .collect::<Vec<&str>>()
            .join(" ");
        assert_eq!(
            strs,
            "true false define fn struct if else new return mut inout () ( ) + - * / { } -> : ; , | <integer literal> <integer literal> <name> <name> <name> <name> <string literal> <character literal>"
        );
        Ok(())
    }

    #[test]
    fn test_invalid_tokens() -> Result<()> {
        let tokens = lex("\"");
        assert!(tokens.is_err());
        let tokens = lex("🤔 🤔 🤔");
        assert!(tokens.is_err());
        Ok(())
    }
}
