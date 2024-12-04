use std::collections::HashMap;
use std::sync::Arc;
use std::sync::LazyLock;

use panic_lang::parser::syntax_tree::Decl;
use panic_lang::parser::syntax_tree::Identifier;
use panic_lang::parser::syntax_tree::SpanPair;

use crate::value::Value;

#[derive(Debug)]
pub struct UserDefinedPrimitiveType {
    pub ident: Identifier,
    pub error: bool,
    pub provenance: bool,
}

impl UserDefinedPrimitiveType {
    pub(crate) fn value(&self, span: SpanPair) -> Value {
        let provenance = if self.provenance { Some(span) } else { None };
        Value::UserPrimitive(crate::value::PrimitiveValue {
            identifier: self.ident.name.clone(),
            error: self.error,
            provenance,
        })
    }
}

pub struct Declarations {
    pub syntax_tree: HashMap<Arc<String>, Decl>,
    pub primitives: HashMap<Arc<String>, UserDefinedPrimitiveType>,
}

pub(crate) static ARITHMETIC_OVERFLOW: LazyLock<Arc<String>> =
    LazyLock::new(|| Arc::new(String::from("ArithmeticOverflow")));
pub(crate) static ARITHMETIC_DIVISION_BY_ZERO: LazyLock<Arc<String>> =
    LazyLock::new(|| Arc::new(String::from("ArithmeticDivisionByZero")));
pub(crate) static STACK_OVERFLOW: LazyLock<Arc<String>> =
    LazyLock::new(|| Arc::new(String::from("StackOverflow")));
pub(crate) static HEAP_OVERFLOW: LazyLock<Arc<String>> =
    LazyLock::new(|| Arc::new(String::from("HeapOverflow")));

impl Declarations {
    // TODO: move this into a panic source file
    fn preamble(primitives: &mut HashMap<Arc<String>, UserDefinedPrimitiveType>) {
        let keys = [
            &*ARITHMETIC_OVERFLOW,
            &*ARITHMETIC_DIVISION_BY_ZERO,
            &*STACK_OVERFLOW,
            &*HEAP_OVERFLOW,
        ];
        for key in keys {
            primitives.insert(
                key.clone(),
                UserDefinedPrimitiveType {
                    ident: Identifier {
                        name: key.clone(),
                        span: SpanPair { start: 0, end: 0 },
                    },
                    error: true,
                    provenance: true,
                },
            );
        }
    }

    pub fn new(syntax_tree: HashMap<Arc<String>, Decl>) -> Declarations {
        let mut primitives = HashMap::new();
        Declarations::preamble(&mut primitives);
        for (key, decl) in syntax_tree.iter() {
            if let Decl::PrimitiveType(decl) = decl {
                let mut error = false;
                let mut provenance = false;
                if let Some(decl_expr) = &decl.relations {
                    for decl_ref in decl_expr.decl.to_vec() {
                        if *decl_ref.identifier().name == "Error" {
                            error = true;
                        } else if *decl_ref.identifier().name == "Provenance" {
                            provenance = true;
                        }
                    }
                }
                primitives.insert(
                    key.clone(),
                    UserDefinedPrimitiveType {
                        ident: decl.ident.clone(),
                        error,
                        provenance,
                    },
                );
            }
        }
        Declarations {
            syntax_tree,
            primitives,
        }
    }
}
