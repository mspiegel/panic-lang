extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate pest_ascii_tree;

mod peg_parser {
    #[derive(Parser)]
    #[grammar = "panic.pest"]
    pub struct PanicParser;
}

mod parser;
mod types;

use std::io;
use std::io::BufRead;

use pest::Parser;
use pest_ascii_tree::print_ascii_tree;

use crate::parser::parse_program;
use crate::peg_parser::*;

fn main() {
    let stdin = io::stdin();
    let lines: Vec<_> = stdin.lock().lines().collect::<Result<_, _>>().unwrap();
    let input = lines.join("\n");

    let pairs = PanicParser::parse(Rule::program, &input);
    if pairs.is_err() {
        panic!("{}", pairs.unwrap_err());
    }

    print_ascii_tree(pairs.clone());

    let prog = parse_program(pairs.unwrap().next().unwrap());
    println!("{:#?}", prog);
}
