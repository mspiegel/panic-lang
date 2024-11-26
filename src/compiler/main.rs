use std::process::ExitCode;

use clap::Subcommand;
use panic_lang::error::PanicLangError;

#[derive(Debug, clap::Parser)]
#[clap(name = "panic", version)]
pub struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// apply pani reductions to the input file and compile
    Build,
}

fn main() -> Result<ExitCode, PanicLangError> {
    let _cli = <Cli as clap::Parser>::parse();
    Ok(ExitCode::SUCCESS)
}
