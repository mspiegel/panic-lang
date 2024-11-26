use num_traits::cast::ToPrimitive;

use panic_lang::error::PanicErrorImpl;
use panic_lang::error::PanicLangError;
use panic_lang::parser::syntax_tree::Expr;
use panic_lang::parser::syntax_tree::ExprType;

use crate::environment::Environment;
use crate::value::Value;

pub enum ErrorOrEarlyReturn {
    Error(PanicLangError),
    EarlyReturn(Value),
}

impl From<PanicErrorImpl> for ErrorOrEarlyReturn {
    fn from(value: PanicErrorImpl) -> Self {
        ErrorOrEarlyReturn::Error(value.into())
    }
}

pub fn evaluate(expr: &Expr, env: &Environment) -> Result<Value, ErrorOrEarlyReturn> {
    let value = match &expr.expr {
        ExprType::IntLiteral(bigval) => match bigval.to_i32() {
            Some(val) => Value::Int32(val),
            None => Value::ArithmeticOverflow(expr.span),
        },
        ExprType::BoolLiteral(val) => Value::Bool(*val),
        ExprType::VarReference(var_ref) => env
            .get(&var_ref.name, var_ref.span)
            .map_err(ErrorOrEarlyReturn::Error)?,
        ExprType::FuncCall(_, _) => todo!(),
        ExprType::Add(exprs) => {
            let values = exprs
                .iter()
                .map(|e| evaluate(e, env))
                .collect::<Result<Vec<_>, _>>()?;
            let mut sum = 0i32;
            for i in 0..exprs.len() {
                let value = &values[i];
                if let Value::Int32(val) = value {
                    if let Some(result) = sum.checked_add(*val) {
                        sum = result;
                    } else {
                        return Ok(Value::ArithmeticOverflow(expr.span));
                    }
                } else {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot add {:?} at {:?}",
                        value, exprs[i].span
                    ))
                    .into());
                }
            }
            Value::Int32(sum)
        }
        ExprType::Sub(lhs_expr, rhs_expr) => {
            let lhs = evaluate(lhs_expr, env)?;
            let rhs = evaluate(rhs_expr, env)?;
            match (lhs, rhs) {
                (Value::Int32(lhs), Value::Int32(rhs)) => {
                    if let Some(result) = lhs.checked_sub(rhs) {
                        Value::Int32(result)
                    } else {
                        Value::ArithmeticOverflow(expr.span)
                    }
                }
                (Value::Int32(_), rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot subtract {:?} at {:?}",
                        rhs, rhs_expr.span,
                    ))
                    .into());
                }
                (lhs, Value::Int32(_)) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot subtract {:?} at {:?}",
                        lhs, lhs_expr.span,
                    ))
                    .into());
                }
                (lhs, rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot subtract {:?} at {:?} and {:?} at {:?}",
                        lhs, lhs_expr.span, rhs, rhs_expr.span,
                    ))
                    .into());
                }
            }
        }
        ExprType::Mul(exprs) => {
            let values = exprs
                .iter()
                .map(|e| evaluate(e, env))
                .collect::<Result<Vec<_>, _>>()?;
            let mut product = 1i32;
            for i in 0..exprs.len() {
                let value = &values[i];
                if let Value::Int32(val) = value {
                    if let Some(result) = product.checked_mul(*val) {
                        product = result;
                    } else {
                        return Ok(Value::ArithmeticOverflow(expr.span));
                    }
                } else {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot multiply {:?} at {:?}",
                        value, exprs[i].span
                    ))
                    .into());
                }
            }
            Value::Int32(product)
        }
        ExprType::Div(lhs_expr, rhs_expr) => {
            let lhs = evaluate(lhs_expr, env)?;
            let rhs = evaluate(rhs_expr, env)?;
            match (lhs, rhs) {
                (Value::Int32(lhs), Value::Int32(rhs)) => {
                    if let Some(result) = lhs.checked_div(rhs) {
                        Value::Int32(result)
                    } else {
                        Value::ArithmeticOverflow(expr.span)
                    }
                }
                (Value::Int32(_), rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot divide {:?} at {:?}",
                        rhs, rhs_expr.span,
                    ))
                    .into());
                }
                (lhs, Value::Int32(_)) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot divide {:?} at {:?}",
                        lhs, lhs_expr.span,
                    ))
                    .into());
                }
                (lhs, rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot divide {:?} at {:?} and {:?} at {:?}",
                        lhs, lhs_expr.span, rhs, rhs_expr.span,
                    ))
                    .into());
                }
            }
        }
        ExprType::Lt(lhs_expr, rhs_expr) => {
            let lhs = evaluate(lhs_expr, env)?;
            let rhs = evaluate(rhs_expr, env)?;
            match (lhs, rhs) {
                (Value::Int32(lhs), Value::Int32(rhs)) => Value::Bool(lhs < rhs),
                (Value::Int32(_), rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot < compare {:?} at {:?}",
                        rhs, rhs_expr.span,
                    ))
                    .into());
                }
                (lhs, Value::Int32(_)) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot < compare {:?} at {:?}",
                        lhs, lhs_expr.span,
                    ))
                    .into());
                }
                (lhs, rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot < compare {:?} at {:?} and {:?} at {:?}",
                        lhs, lhs_expr.span, rhs, rhs_expr.span,
                    ))
                    .into());
                }
            }
        }
        ExprType::LtEq(lhs_expr, rhs_expr) => {
            let lhs = evaluate(lhs_expr, env)?;
            let rhs = evaluate(rhs_expr, env)?;
            match (lhs, rhs) {
                (Value::Int32(lhs), Value::Int32(rhs)) => Value::Bool(lhs <= rhs),
                (Value::Int32(_), rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot <= compare {:?} at {:?}",
                        rhs, rhs_expr.span,
                    ))
                    .into());
                }
                (lhs, Value::Int32(_)) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot <= compare {:?} at {:?}",
                        lhs, lhs_expr.span,
                    ))
                    .into());
                }
                (lhs, rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot <= compare {:?} at {:?} and {:?} at {:?}",
                        lhs, lhs_expr.span, rhs, rhs_expr.span,
                    ))
                    .into());
                }
            }
        }
        ExprType::Eq(lhs_expr, rhs_expr) => {
            let lhs = evaluate(lhs_expr, env)?;
            let rhs = evaluate(rhs_expr, env)?;
            match (lhs, rhs) {
                (Value::Int32(lhs), Value::Int32(rhs)) => Value::Bool(lhs == rhs),
                (Value::Int32(_), rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot == compare {:?} at {:?}",
                        rhs, rhs_expr.span,
                    ))
                    .into());
                }
                (lhs, Value::Int32(_)) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot == compare {:?} at {:?}",
                        lhs, lhs_expr.span,
                    ))
                    .into());
                }
                (lhs, rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot == compare {:?} at {:?} and {:?} at {:?}",
                        lhs, lhs_expr.span, rhs, rhs_expr.span,
                    ))
                    .into());
                }
            }
        }
        ExprType::NotEq(lhs_expr, rhs_expr) => {
            let lhs = evaluate(lhs_expr, env)?;
            let rhs = evaluate(rhs_expr, env)?;
            match (lhs, rhs) {
                (Value::Int32(lhs), Value::Int32(rhs)) => Value::Bool(lhs != rhs),
                (Value::Int32(_), rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot != compare {:?} at {:?}",
                        rhs, rhs_expr.span,
                    ))
                    .into());
                }
                (lhs, Value::Int32(_)) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot != compare {:?} at {:?}",
                        lhs, lhs_expr.span,
                    ))
                    .into());
                }
                (lhs, rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot != compare {:?} at {:?} and {:?} at {:?}",
                        lhs, lhs_expr.span, rhs, rhs_expr.span,
                    ))
                    .into());
                }
            }
        }
        ExprType::GtEq(lhs_expr, rhs_expr) => {
            let lhs = evaluate(lhs_expr, env)?;
            let rhs = evaluate(rhs_expr, env)?;
            match (lhs, rhs) {
                (Value::Int32(lhs), Value::Int32(rhs)) => Value::Bool(lhs >= rhs),
                (Value::Int32(_), rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot >= compare {:?} at {:?}",
                        rhs, rhs_expr.span,
                    ))
                    .into());
                }
                (lhs, Value::Int32(_)) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot >= compare {:?} at {:?}",
                        lhs, lhs_expr.span,
                    ))
                    .into());
                }
                (lhs, rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot >= compare {:?} at {:?} and {:?} at {:?}",
                        lhs, lhs_expr.span, rhs, rhs_expr.span,
                    ))
                    .into());
                }
            }
        }
        ExprType::Gt(lhs_expr, rhs_expr) => {
            let lhs = evaluate(lhs_expr, env)?;
            let rhs = evaluate(rhs_expr, env)?;
            match (lhs, rhs) {
                (Value::Int32(lhs), Value::Int32(rhs)) => Value::Bool(lhs > rhs),
                (Value::Int32(_), rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot > compare {:?} at {:?}",
                        rhs, rhs_expr.span,
                    ))
                    .into());
                }
                (lhs, Value::Int32(_)) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot > compare {:?} at {:?}",
                        lhs, lhs_expr.span,
                    ))
                    .into());
                }
                (lhs, rhs) => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot > compare {:?} at {:?} and {:?} at {:?}",
                        lhs, lhs_expr.span, rhs, rhs_expr.span,
                    ))
                    .into());
                }
            }
        }
        ExprType::And(exprs) => {
            let values = exprs
                .iter()
                .map(|e| evaluate(e, env))
                .collect::<Result<Vec<_>, _>>()?;
            let mut result = true;
            for i in 0..exprs.len() {
                let value = &values[i];
                if let Value::Bool(val) = value {
                    result &= val;
                } else {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot apply logical and {:?} at {:?}",
                        value, exprs[i].span
                    ))
                    .into());
                }
            }
            Value::Bool(result)
        }
        ExprType::Or(exprs) => {
            let values = exprs
                .iter()
                .map(|e| evaluate(e, env))
                .collect::<Result<Vec<_>, _>>()?;
            let mut result = false;
            for i in 0..exprs.len() {
                let value = &values[i];
                if let Value::Bool(val) = value {
                    result &= val;
                } else {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot apply logical and {:?} at {:?}",
                        value, exprs[i].span
                    ))
                    .into());
                }
            }
            Value::Bool(result)
        }
        ExprType::Negate(operand) => {
            let val = evaluate(operand, env)?;
            match val {
                Value::Int32(val) => {
                    if let Some(result) = val.checked_neg() {
                        Value::Int32(result)
                    } else {
                        Value::ArithmeticOverflow(expr.span)
                    }
                }
                val => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot negate {:?} at {:?}",
                        val, operand.span,
                    ))
                    .into());
                }
            }
        }
        ExprType::Not(operand) => {
            let val = evaluate(operand, env)?;
            match val {
                Value::Bool(val) => Value::Bool(!val),
                val => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot logical negate {:?} at {:?}",
                        val, operand.span,
                    ))
                    .into());
                }
            }
        }
        ExprType::Question(inner) => {
            let val = evaluate(inner, env)?;
            return Err(ErrorOrEarlyReturn::EarlyReturn(val));
        }
        ExprType::Paren(inner) => evaluate(inner, env)?,
    };
    Ok(value)
}
