use std::fmt;
use std::sync::Arc;

use num_bigint::BigInt;

use panic_lang::parser::syntax_tree::SpanPair;

use crate::declaration::ARITHMETIC_DIVISION_BY_ZERO;
use crate::declaration::ARITHMETIC_OVERFLOW;

#[derive(Clone)]
pub enum Value {
    IntLiteral(Arc<BigInt>),
    BoolLiteral(bool),
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

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IntLiteral(val) => write!(f, "{}", val),
            Self::BoolLiteral(val) => write!(f, "{}", val),
            Self::Int32(val) => write!(f, "{}", val),
            Self::Bool(val) => write!(f, "{}", val),
            Self::UserPrimitive(val) => match &val.provenance {
                Some(span) => write!(f, "{} at {:?}", val.identifier, span),
                None => write!(f, "{}", val.identifier),
            },
        }
    }
}
