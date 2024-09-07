use std::fmt::Display;
use std::fmt::Formatter;

use crate::parser::syntax_tree::*;

fn indent(formatter: &mut Formatter<'_>, level: usize) -> std::fmt::Result {
    for _ in 0..level {
        write!(formatter, "    ")?;
    }
    Ok(())
}

impl Display for Program {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let precision = formatter.precision().unwrap_or_default();
        for decl in self.decls.iter() {
            write!(formatter, "{:.*}", precision, decl)?;
        }
        Ok(())
    }
}

impl Display for TopDecl {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let precision = formatter.precision().unwrap_or_default();
        match self {
            TopDecl::Func(func) => write!(formatter, "{:.*}", precision, func),
        }
    }
}

fn fmt_slice<T: Display>(
    elements: &[T],
    sep: &str,
    formatter: &mut Formatter<'_>,
) -> std::fmt::Result {
    for i in 0..elements.len() {
        write!(formatter, "{}", elements[i])?;
        if i < (elements.len() - 1) {
            write!(formatter, "{}", sep)?;
        }
    }
    Ok(())
}

impl Display for FunctionDecl {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let precision = formatter.precision().unwrap_or_default();
        indent(formatter, precision)?;
        write!(formatter, "_fn_ {}(", self.ident)?;
        fmt_slice(&self.params, ", ", formatter)?;
        writeln!(formatter, ") -> {} {{", self.return_type)?;
        for stmt in self.stmts.iter() {
            write!(formatter, "{:.*}", precision + 1, stmt)?;
        }
        indent(formatter, precision)?;
        write!(formatter, "}}\n\n")?;
        Ok(())
    }
}

impl Display for FuncParamDecl {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}: {}", self.ident, self.type_expr)
    }
}

impl Display for Stmt {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let precision = formatter.precision().unwrap_or_default();
        indent(formatter, precision)?;
        match self {
            Stmt::Empty(_) => writeln!(formatter, ";"),
            Stmt::Let(stmt) => writeln!(formatter, "{:.*};", precision, stmt),
            Stmt::Return(stmt) => writeln!(formatter, "{:.*};", precision, stmt),
            Stmt::If(stmt) => write!(formatter, "{:.*}", precision, stmt),
            Stmt::Expr(stmt) => writeln!(formatter, "{:.*};", precision, stmt),
        }
    }
}

impl Display for LetStmt {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "_let_ {}: {} = {}",
            self.ident, self.type_expr, self.expr
        )
    }
}

impl Display for ReturnStmt {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "_return_ {}", self.expr)
    }
}

impl Display for IfStmt {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let precision = formatter.precision().unwrap_or_default();
        writeln!(formatter, "_if_ {} {{", self.test)?;
        for stmt in self.then_statements.iter() {
            write!(formatter, "{:.*}", precision + 1, stmt)?;
        }
        indent(formatter, precision)?;
        match &self.else_statements {
            Else::ElseIf(if_stmt) => write!(formatter, "}} _else_ {:.*}", precision, if_stmt)?,
            Else::ElseStatements(stmts) => {
                writeln!(formatter, "}} _else_ {{")?;
                for stmt in stmts.iter() {
                    write!(formatter, "{:.*}", precision + 1, stmt)?;
                }
                indent(formatter, precision)?;
                writeln!(formatter, "}}")?;
            }
            Else::Empty() => writeln!(formatter, "}}")?,
        }
        Ok(())
    }
}

impl Display for ExprStmt {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", self.expr)
    }
}

impl Display for TypeExpr {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.typ {
            TypeExprEnum::Ref(reference) => write!(formatter, "{}", reference),
            TypeExprEnum::Union(exprs) => {
                write!(formatter, "( ")?;
                fmt_slice(exprs, " | ", formatter)?;
                write!(formatter, " )")
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.expr {
            ExprType::IntLiteral(i) => write!(formatter, "{}", i),
            ExprType::BoolLiteral(b) => write!(formatter, "{}", b),
            ExprType::VarReference(iden) => write!(formatter, "{}", iden),
            ExprType::FuncCall(iden, args) => {
                write!(formatter, "{}(", iden)?;
                fmt_slice(args, ", ", formatter)?;
                write!(formatter, ")")
            }
            ExprType::Add(exprs) => fmt_slice(exprs, " + ", formatter),
            ExprType::Sub(lhs, rhs) => write!(formatter, "{} - {}", lhs, rhs),
            ExprType::Mul(exprs) => fmt_slice(exprs, " * ", formatter),
            ExprType::Div(lhs, rhs) => write!(formatter, "{} / {}", lhs, rhs),
            ExprType::And(exprs) => fmt_slice(exprs, " && ", formatter),
            ExprType::Or(exprs) => fmt_slice(exprs, " || ", formatter),
            ExprType::Negate(expr) => write!(formatter, "-{}", expr),
            ExprType::Not(expr) => write!(formatter, "!{}", expr),
            ExprType::Question(expr) => write!(formatter, "{}?", expr),
            ExprType::Paren(expr) => write!(formatter, "({})", expr),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", self.name)
    }
}

impl Display for TypeRef {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeRef::TypeName(identifier) => write!(formatter, "{}", identifier),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Read;

    use crate::parser::peg_grammar::PanicParser;
    use crate::parser::peg_grammar::Rule;

    use super::*;

    #[test]
    fn test_roundtrip() {
        let path = "tests/valid";
        let entries = std::fs::read_dir(path).unwrap();
        for entry in entries {
            let entry = entry.expect("error reading directory");
            let mut file = std::fs::File::open(entry.path()).expect("error opening file");
            let mut input = Vec::new();
            file.read_to_end(&mut input).expect("error reading file");
            let input = String::from_utf8(input).expect("error converting file to string");
            let mut pairs = <PanicParser as pest::Parser<_>>::parse(Rule::program, &input)
                .expect("error parsing file");
            let top_node = pairs.next().unwrap();
            let syntax_tree = program(top_node).expect("error generating syntax tree");
            let text = format!("{}", syntax_tree);
            let mut pairs = <PanicParser as pest::Parser<_>>::parse(Rule::program, &text)
                .expect("error parsing file");
            let top_node = pairs.next().unwrap();
            assert!(program(top_node).is_ok());
        }
    }
}
