use std::io;
use std::io::BufRead;

use pest::Parser;
use pest_ascii_tree::print_ascii_tree;

use panic_lang::parser::peg_grammar::*;
use panic_lang::parser::syntax_tree::*;

fn main() {
    let stdin = io::stdin();
    let lines: Vec<_> = stdin.lock().lines().collect::<Result<_, _>>().unwrap();
    let input = lines.join("\n");

    let pairs = PanicParser::parse(Rule::program, &input);
    if pairs.is_err() {
        panic!("{}", pairs.unwrap_err());
    }

    print_ascii_tree(pairs.clone());

    let program = program(pairs.into_iter().next().unwrap().next().unwrap());

    println!("syntax tree {:#?}", program);
}
