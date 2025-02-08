use std::io;

use logos::Logos;

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

fn main() -> io::Result<()> {
    let input = io::read_to_string(io::stdin())?;
    let lexer = Token::lexer(&input);
    for token in lexer {
        println!("{:?}", token);
    }
    Ok(())
}
