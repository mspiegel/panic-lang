use std::io;

use miette::IntoDiagnostic;
use miette::Report;

use panic_lang::lexer::lex;

fn main_helper(input: &str) -> panic_lang::errors::Result<()> {
    let tokens = lex(input)?;
    println!("{:?}", tokens.iter().map(|x| &x.token).collect::<Vec<_>>());
    //    let program = parse(input, tokens)?;
    //    println!("{:?}", program);
    Ok(())
}

fn main() -> miette::Result<()> {
    let input = io::read_to_string(io::stdin()).into_diagnostic()?;
    main_helper(&input).map_err(|error| Into::<Report>::into(error).with_source_code(input))
}
