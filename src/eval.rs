use crate::errors::Result;
use crate::parser::Expr;
use crate::parser::ExprContents;
use crate::value::Value;

pub fn eval_expr(expr: &Expr) -> Result<Value> {
    Ok(Value::EmptyList)
}
