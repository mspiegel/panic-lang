use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "panic.pest"]
pub struct PanicParser;
