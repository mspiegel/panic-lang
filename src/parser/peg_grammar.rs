use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "panic.pest"]
pub struct PanicParser;

#[cfg(test)]
mod tests {
    use std::io::Read;

    use crate::parser::peg_grammar::PanicParser;

    use super::*;

    #[test]
    fn test_valid() {
        let path = "tests/valid";
        let entries = std::fs::read_dir(path).unwrap();
        for entry in entries {
            let entry = entry.expect("error reading directory");
            let mut file = std::fs::File::open(entry.path()).expect("error opening file");
            let mut input = Vec::new();
            file.read_to_end(&mut input).expect("error reading file");
            let input = String::from_utf8(input).expect("error converting file to string");
            _ = <PanicParser as pest::Parser<_>>::parse(Rule::program, &input)
                .expect("error parsing file");
        }
    }
}
