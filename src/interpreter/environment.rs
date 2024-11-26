use std::collections::HashMap;

use panic_lang::error::PanicErrorImpl;
use panic_lang::error::PanicLangError;
use panic_lang::parser::syntax_tree::SpanPair;

use crate::value::Value;

pub struct Environment {
    values: HashMap<String, Value>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn get(&self, key: &str, span: SpanPair) -> Result<Value, PanicLangError> {
        if let Some(val) = self.values.get(key) {
            Ok(val.clone())
        } else if let Some(parent) = self.parent.as_ref() {
            parent.get(key, span)
        } else {
            Err(PanicErrorImpl::EvaluationError(format!(
                "cannot find variable '{}' at {:?}",
                key, span
            ))
            .into())
        }
    }
}
