use ordermap::OrderMap;

use crate::errors::Result;
use crate::parser::Expr;
use crate::parser::ExprContents;
use crate::parser::Identifier;
use crate::value::Value;

#[derive(Default)]
pub struct TopLevel {
    pub values: OrderMap<Identifier, Value>,
}

#[derive(Default)]
pub struct LocalScope {
    pub values: Vec<OrderMap<Identifier, Value>>,
}

impl TopLevel {
    pub fn get(&self, key: &Identifier) -> Option<Value> {
        let candidate = self.values.get(key);
        if candidate.is_some() {
            candidate.cloned()
        } else {
            None
        }
    }
}

impl LocalScope {
    pub fn get_mut(&mut self, key: &Identifier) -> Option<&mut Value> {
        for level in self.values.iter_mut().rev() {
            let candidate = level.get_mut(key);
            if candidate.is_some() {
                return candidate;
            }
        }
        None
    }

    pub fn get(&self, key: &Identifier) -> Option<Value> {
        for level in self.values.iter().rev() {
            let candidate = level.get(key);
            if candidate.is_some() {
                return candidate.cloned();
            }
        }
        None
    }
}

pub fn eval(expr: &Expr, locals: &mut LocalScope, globals: &mut TopLevel) -> Result<Value> {
    match &expr.contents {
        ExprContents::EmptyList => return Ok(Value::EmptyList),
        ExprContents::BoolLiteral(val) => return Ok(Value::Boolean(*val)),
        ExprContents::CharLiteral(val) => return Ok(Value::Character(*val)),
        ExprContents::StringLiteral(val) => return Ok(Value::String(val.clone())),
        ExprContents::IntLiteral(val) => {
            return Ok(Value::Integer(val.clone()));
        }
        ExprContents::Reference(identifier) => {
            let val = locals.get(identifier);
            if let Some(val) = val {
                return Ok(val);
            }
            let val = globals.get(identifier);
            if let Some(val) = val {
                return Ok(val);
            }
        }
        ExprContents::SetBang { lvalue, rvalue } => {
            let val = eval(rvalue, locals, globals)?;
            if let ExprContents::Reference(identifier) = &lvalue.contents {
                if let Some(variable) = locals.get_mut(identifier) {
                    *variable = val;
                    return Ok(Value::EmptyList);
                }
                if let Some(variable) = globals.values.get_mut(identifier) {
                    *variable = val;
                    return Ok(Value::EmptyList);
                }
            }
        }
        ExprContents::LetStar { bindings, body } => {
            let level = locals.values.len();
            locals.values.push(OrderMap::new());
            for bind in bindings {
                let k = &bind.identifier;
                let v = eval(&bind.body, locals, globals)?;
                locals.values[level].insert(k.clone(), v);
            }
            let result = eval(body, locals, globals)?;
            locals.values.pop();
            return Ok(result);
        }
        _ => todo!(),
    }
    Ok(Value::EmptyList)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::parse_expr;

    fn parse_input_expr(input: &str) -> Result<Expr> {
        let tokens = lex(input)?;
        let mut iter = tokens.into_iter().peekable();
        parse_expr(input, &mut iter)
    }

    fn eval_input_expr(input: &str) -> Result<Value> {
        let expr = parse_input_expr(input)?;
        let mut locals = LocalScope::default();
        let mut globals = TopLevel::default();
        eval(&expr, &mut locals, &mut globals)
    }

    #[test]
    fn test_eval_expr() -> Result<()> {
        assert_eq!(
            eval_input_expr("(let* ((a 0)) a)")?,
            Value::Integer(0.into())
        );
        assert_eq!(
            eval_input_expr("(let* ((a 0) (b a)) b)")?,
            Value::Integer(0.into())
        );
        assert_eq!(
            eval_input_expr("(let* ((a 0) (b (set! a 1))) a)")?,
            Value::Integer(1.into())
        );
        assert_eq!(
            eval_input_expr("(let* ((a 0) (b (set! a 1))) b)")?,
            Value::EmptyList
        );
        Ok(())
    }
}
