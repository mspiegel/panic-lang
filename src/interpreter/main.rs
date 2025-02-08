use std::io;

use miette::IntoDiagnostic;
use miette::Result;

use panic_lang::lexer::lex;
use panic_lang::parser::parse;

fn main_helper(input: &str) -> Result<()> {
    let tokens = lex(input)?;
    println!("{:?}", tokens);
    let program = parse(tokens);
    println!("{:?}", program);
    Ok(())
}

fn main() -> Result<()> {
    let input = io::read_to_string(io::stdin()).into_diagnostic()?;
    main_helper(&input).map_err(|error| error.with_source_code(input))
}
