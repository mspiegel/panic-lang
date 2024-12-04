use std::collections::HashMap;
use std::sync::Arc;

use panic_lang::error::PanicErrorImpl;
use panic_lang::error::PanicLangError;
use panic_lang::parser::syntax_tree::SpanPair;

use crate::value::Value;

pub struct Environment<'a> {
    values: HashMap<Arc<String>, Value>,
    parent: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new(parent: Option<&'a Environment>) -> Environment<'a> {
        Environment {
            values: HashMap::new(),
            parent,
        }
    }
    pub fn get(&self, key: &Arc<String>, span: SpanPair) -> Result<Value, PanicLangError> {
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

    pub fn set(&mut self, key: Arc<String>, val: Value) {
        self.values.insert(key.clone(), val);
    }
}
