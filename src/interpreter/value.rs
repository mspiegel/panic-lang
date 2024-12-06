use std::fmt;
use std::sync::Arc;

use panic_lang::parser::syntax_tree::SpanPair;

use crate::declaration::ARITHMETIC_DIVISION_BY_ZERO;
use crate::declaration::ARITHMETIC_OVERFLOW;

#[derive(Clone)]
pub enum Value {
    Int32(i32),
    Bool(bool),
    UserPrimitive(PrimitiveValue),
}

#[derive(Clone)]
pub struct PrimitiveValue {
    pub identifier: Arc<String>,
    pub error: bool,
    pub provenance: Option<SpanPair>,
}

pub fn arithmetic_overflow(provenance: SpanPair) -> Value {
    Value::UserPrimitive(PrimitiveValue {
        identifier: (*ARITHMETIC_OVERFLOW).clone(),
        error: true,
        provenance: Some(provenance),
    })
}

pub fn arithmetic_division_by_zero(provenance: SpanPair) -> Value {
    Value::UserPrimitive(PrimitiveValue {
        identifier: (*ARITHMETIC_DIVISION_BY_ZERO).clone(),
        error: true,
        provenance: Some(provenance),
    })
}

impl Value {
    pub fn is_error(&self) -> bool {
        match self {
            Value::UserPrimitive(val) => val.error,
            _ => false,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int32(i), Value::Int32(j)) => i == j,
            (Value::Bool(i), Value::Bool(j)) => i == j,
            (Value::UserPrimitive(i), Value::UserPrimitive(j)) => i.identifier == j.identifier,
            (_, _) => unreachable!(),
        }
    }
}

impl Eq for Value {}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int32(val) => write!(f, "{}", val),
            Self::Bool(val) => write!(f, "{}", val),
            Self::UserPrimitive(val) => match &val.provenance {
                Some(span) => write!(f, "{} at {:?}", val.identifier, span),
                None => write!(f, "{}", val.identifier),
            },
        }
    }
}
