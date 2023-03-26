extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate pest_ascii_tree;

use std::io;
use std::io::BufRead;

use pest::Parser;
use pest_ascii_tree::print_ascii_tree;

#[derive(Parser)]
#[grammar = "panic.pest"] // relative to src
struct PanicParser;

fn main() {

    let stdin = io::stdin();
    let lines: Vec<_> = stdin.lock().lines().collect::<Result<_, _>>().unwrap();
    let input = lines.join("\n");

    print_ascii_tree(
        PanicParser::parse(
            Rule::program,
            &input,
        )
    );

}
