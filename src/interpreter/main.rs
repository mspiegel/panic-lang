use std::io;
use std::io::BufRead;

//use pest_ascii_tree::print_ascii_tree;
use clap::Subcommand;

use panic_lang::error::PanicLangError;
use panic_lang::parser::peg_grammar::PanicParser;
use panic_lang::parser::peg_grammar::Rule;
use panic_lang::parser::syntax_tree::program;
use panic_lang::parser::syntax_tree::Program;

#[derive(Debug, clap::Parser)]
#[clap(name = "pani", version)]
pub struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// parse the input file, and pretty-print to standard output
    Print,
    /// parse the input file, apply reductions, and pretty-print to standard output
    Reduce,
    /// parse the input file, and run the program
    Run,
}

fn command_print(_input: &str, prog: Program) {
    print!("{}", prog);
}

fn main() -> Result<(), PanicLangError> {
    let cli = <Cli as clap::Parser>::parse();

    let stdin = io::stdin();
    let lines: Vec<_> = stdin.lock().lines().collect::<Result<_, _>>().unwrap();
    let input = lines.join("\n");

    let mut pairs = <PanicParser as pest::Parser<_>>::parse(Rule::program, &input)?;
    let top_node = pairs.next().unwrap();
    //    print_ascii_tree(top_node.clone());
    let prog = program(top_node)?;

    match cli.command {
        Command::Print => command_print(&input, prog),
        Command::Reduce => todo!(),
        Command::Run => todo!(),
    }
    Ok(())
}
