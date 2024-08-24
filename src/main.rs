mod peg_grammar {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "panic.pest"]
    pub struct PanicParser;
}

use std::io;
use std::io::BufRead;

use pest::Parser;
use pest_ascii_tree::print_ascii_tree;

use crate::peg_grammar::*;

fn main() {
    let stdin = io::stdin();
    let lines: Vec<_> = stdin.lock().lines().collect::<Result<_, _>>().unwrap();
    let input = lines.join("\n");

    let pairs = PanicParser::parse(Rule::program, &input);
    if pairs.is_err() {
        panic!("{}", pairs.unwrap_err());
    }

    print_ascii_tree(pairs.clone());
}
