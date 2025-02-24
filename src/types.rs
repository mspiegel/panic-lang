use std::collections::HashMap;
use std::collections::HashSet;

use crate::errors::CompilerErrorDuplicateDefinition;
use crate::errors::CompilerErrorNoDefinition;
use crate::errors::PanicLangError;
use crate::errors::ParserErrorNotATypeExpression;
use crate::errors::Result;

use crate::parser::Expr;
use crate::parser::ExprContents;
use crate::parser::Identifier;
use crate::parser::Program;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Undetermined,
    EmptyList,
    Boolean,
    Integer,
    Character,
    LiteralString,
    Marker {
        uuid: u64,
    },
    // Value
    // Reference
    // Resource
    Newtype {
        uuid: u64,
        inner: Box<Type>,
    },
    Ref(Box<Type>),
    Slice(Box<Type>),
    Function {
        arguments: Vec<Type>,
        retval: Box<Type>,
    },
    Union {
        default: Box<Type>,
        variants: Vec<Type>,
    },
    Meta,
}

// TODO prevent infinite recursion
pub fn build_type_from_type_expr(
    type_expr: &Expr,
    program: &Program,
    definitions: &mut HashMap<Identifier, Type>,
) -> Result<Type, PanicLangError> {
    match &type_expr.contents {
        ExprContents::EmptyList => {
            return Ok(Type::EmptyList);
        }
        ExprContents::Reference(identifier) => {
            let primitive = match identifier.0.as_str() {
                "i32" => Some(Type::Integer),
                "bool" => Some(Type::Boolean),
                "char" => Some(Type::Character),
                _ => None,
            };
            if let Some(primitive) = primitive {
                return Ok(primitive);
            }
            if let Some(typ) = definitions.get(identifier) {
                return Ok(typ.clone());
            }
            let definition = program
                .definitions
                .iter()
                .filter(|def| def.identifier.identifier == *identifier)
                .next();
            match definition {
                Some(definition) => {
                    let typ = build_type_from_type_expr(
                        &definition.identifier.type_expr,
                        program,
                        definitions,
                    )?;
                    definitions.insert(identifier.clone(), typ.clone());
                    return Ok(typ);
                }
                None => {
                    return Err(PanicLangError::CompilerErrorNoDefinition(
                        CompilerErrorNoDefinition { at: type_expr.at },
                    ))
                }
            }
        }
        ExprContents::Slice(expr) => {
            let typ = Box::new(build_type_from_type_expr(expr, program, definitions)?);
            return Ok(Type::Slice(typ));
        }
        ExprContents::RArrow {
            argument_types,
            return_type,
        } => {
            let mut arguments = vec![];
            for type_expr in argument_types {
                arguments.push(build_type_from_type_expr(type_expr, program, definitions)?);
            }
            let retval = Box::new(build_type_from_type_expr(
                return_type,
                program,
                definitions,
            )?);
            Ok(Type::Function { arguments, retval })
        }
        _ => Err(PanicLangError::ParserErrorNotATypeExpression(
            ParserErrorNotATypeExpression { at: type_expr.at },
        )),
    }
}

pub fn build_definitions(program: &Program) -> Result<HashMap<Identifier, Type>, PanicLangError> {
    let mut definitions = HashMap::new();
    let mut names = HashSet::new();
    for definition in &program.definitions {
        if !names.insert(&definition.identifier.identifier) {
            return Err(PanicLangError::CompilerErrorDuplicateDefinition(
                CompilerErrorDuplicateDefinition {
                    at: definition.identifier.at,
                },
            ));
        }
    }
    for definition in &program.definitions {
        let name = &definition.identifier.identifier;
        if !definitions.contains_key(name) {
            let typ = build_type_from_type_expr(
                &definition.identifier.type_expr,
                program,
                &mut definitions,
            )?;
            definitions.insert(name.clone(), typ);
        }
    }
    Ok(definitions)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::parse;

    fn parse_input_program(input: &str) -> Result<Program> {
        let tokens = lex(input)?;
        parse(input, tokens)
    }

    #[test]
    fn test_build_definitions() -> Result<()> {
        let program = parse_input_program(
            "
        (define (: foo (-> (i32) i32)) (lambda (x) x))
        (define (: main (-> () ())) (lambda () ()))
        ",
        )?;
        let definitions = build_definitions(&program)?;
        assert_eq!(2, definitions.len());
        assert_eq!(
            definitions.get(&Identifier("foo".into())),
            Some(&Type::Function {
                arguments: vec![Type::Integer],
                retval: Box::new(Type::Integer),
            })
        );
        assert_eq!(
            definitions.get(&Identifier("main".into())),
            Some(&Type::Function {
                arguments: vec![],
                retval: Box::new(Type::EmptyList),
            })
        );
        Ok(())
    }
}
