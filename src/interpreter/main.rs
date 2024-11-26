use std::collections::HashMap;
use std::io;
use std::io::BufRead;
use std::process::ExitCode;

//use pest_ascii_tree::print_ascii_tree;
use clap::Subcommand;

use panic_lang::error::PanicErrorImpl;
use panic_lang::error::PanicLangError;
use panic_lang::parser::peg_grammar::PanicParser;
use panic_lang::parser::peg_grammar::Rule;
use panic_lang::parser::syntax_tree::program;
use panic_lang::parser::syntax_tree::Decl;
use panic_lang::parser::syntax_tree::Program;

pub(crate) mod environment;
pub(crate) mod evaluate;
pub(crate) mod value;

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

fn command_print(_input: &str, prog: Program) -> Result<ExitCode, PanicLangError> {
    print!("{}", prog);
    Ok(ExitCode::SUCCESS)
}

fn command_run_main(
    _input: &str,
    declarations: &HashMap<String, Decl>,
) -> Result<ExitCode, PanicLangError> {
    Ok(ExitCode::SUCCESS)
}

fn command_run(_input: &str, prog: Program) -> Result<ExitCode, PanicLangError> {
    let declarations = prog
        .decls
        .into_iter()
        .map(|decl| (decl.identifier().name.clone(), decl))
        .collect::<HashMap<String, Decl>>();
    match declarations.get("main") {
        Some(Decl::Func(func)) => {
            if func.params.is_empty() {
                command_run_main(_input, &declarations)
            } else {
                Err(PanicErrorImpl::EvaluationError(
                    "_decl_ main _fn_ () has unexpected parameters".into(),
                )
                .into())
            }
        }
        None => Err(PanicErrorImpl::EvaluationError("_decl_ main _fn_ () not found".into()).into()),
    }
}

fn main() -> Result<ExitCode, PanicLangError> {
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
        Command::Run => command_run(&input, prog),
    }
}
