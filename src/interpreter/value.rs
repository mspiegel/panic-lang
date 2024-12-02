use std::fmt;
use std::sync::Arc;

use num_bigint::BigInt;

use panic_lang::parser::syntax_tree::SpanPair;

#[derive(Clone)]
pub enum Value {
    IntLiteral(Arc<BigInt>),
    BoolLiteral(bool),
    Int32(i32),
    Bool(bool),
    ArithmeticOverflow(SpanPair),
    ArithmeticDivisionByZero(SpanPair),
    StackOverflow(SpanPair),
    UserPrimitive(PrimitiveValue),
}

#[derive(Clone)]
pub struct PrimitiveValue {
    pub identifier: String,
    pub error: bool,
    pub provenance: Option<SpanPair>,
}

impl Value {
    pub fn is_error(&self) -> bool {
        match self {
            Value::ArithmeticOverflow(_) => true,
            Value::ArithmeticDivisionByZero(_) => true,
            Value::StackOverflow(_) => true,
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
            Self::ArithmeticOverflow(span) => write!(f, "ArithmeticOverflow at {:?}", span),
            Self::ArithmeticDivisionByZero(span) => {
                write!(f, "ArithmeticDivisionByZero at {:?}", span)
            }
            Self::StackOverflow(span) => write!(f, "StackOverflow at {:?}", span),
            Self::UserPrimitive(val) => match &val.provenance {
                Some(span) => write!(f, "{} at {:?}", val.identifier, span),
                None => write!(f, "{}", val.identifier),
            },
        }
    }
}
