extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate pest_ascii_tree;

use std::io;
use std::io::BufRead;

use pest::iterators::Pair;
use pest::Parser;
use pest_ascii_tree::print_ascii_tree;

#[derive(Parser)]
#[grammar = "panic.pest"]
struct PanicParser;

struct Program {
    decls: Vec<TopDecl>,
}
enum TopDecl {
    Func(Function),
}
struct Function {
    name: String,
    params: Vec<FuncParam>,
    typ: TypeName,
    body: String,
}
struct FuncParam {
    name: String,
    typ: TypeName,
}
enum TypeName {
    Invalid,
    Int32,
    Bool,
    Unit,
}

fn parseProgram(pair: Pair<Rule>) -> Program {
    assert_eq!(pair.as_rule(), Rule::program);
    let mut decls = Vec::new();
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::topdecl => decls.push(parseTopDecl(inner)),
            _ => (),
        }
    }
    return Program { decls };
}

fn parseTopDecl(pair: Pair<Rule>) -> TopDecl {
    assert_eq!(pair.as_rule(), Rule::topdecl);
    return TopDecl::Func(parseFunction(pair.into_inner().next().unwrap()));
}

fn parseFunction(pair: Pair<Rule>) -> Function {
    assert_eq!(pair.as_rule(), Rule::function);
    let mut name = "".to_string();
    let mut params = Vec::new();
    let mut typ = TypeName::Invalid;
    let mut body = "".to_string();
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::ident => name = inner.as_str().to_string(),
            Rule::stmtBlock => body = inner.as_str().to_string(),
            _ => (),
        }
    }
    return Function {
        name,
        params,
        typ,
        body,
    };
}

fn main() {
    let stdin = io::stdin();
    let lines: Vec<_> = stdin.lock().lines().collect::<Result<_, _>>().unwrap();
    let input = lines.join("\n");

    print_ascii_tree(PanicParser::parse(Rule::program, &input));

    let pairs = PanicParser::parse(Rule::program, &input);
    if pairs.is_err() {
        panic!("{}", pairs.unwrap_err());
    }
    _ = parseProgram(pairs.unwrap().next().unwrap());
}
