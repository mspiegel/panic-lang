use std::io;
use std::io::BufRead;

use pest::Parser;
//use pest_ascii_tree::print_ascii_tree;

use panic_lang::parser::peg_grammar::PanicParser;
use panic_lang::parser::peg_grammar::Rule;
use panic_lang::parser::syntax_tree::program;
use panic_lang::parser::syntax_tree::Program;

fn command_print(_input: &str, prog: Program) {
    print!("{}", prog);
}

fn main() {
    let stdin = io::stdin();
    let lines: Vec<_> = stdin.lock().lines().collect::<Result<_, _>>().unwrap();
    let input = lines.join("\n");

    let pairs = PanicParser::parse(Rule::program, &input);
    if pairs.is_err() {
        panic!("{}", pairs.unwrap_err());
    }
    let top_node = pairs.unwrap().next().unwrap();
    //    print_ascii_tree(top_node.clone());
    let prog = program(top_node);
    command_print(&input, prog);
}
