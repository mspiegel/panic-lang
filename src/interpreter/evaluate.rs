use std::collections::HashMap;

use num_traits::cast::ToPrimitive;

use panic_lang::error::PanicErrorImpl;
use panic_lang::error::PanicLangError;
use panic_lang::parser::syntax_tree::Decl;
use panic_lang::parser::syntax_tree::Else;
use panic_lang::parser::syntax_tree::Expr;
use panic_lang::parser::syntax_tree::ExprType;
use panic_lang::parser::syntax_tree::FunctionDecl;
use panic_lang::parser::syntax_tree::IfStmt;
use panic_lang::parser::syntax_tree::Stmt;

use crate::environment::Environment;
use crate::value::Value;

pub enum ErrorOrReturn {
    Error(PanicLangError),
    Return(Value),
}

impl From<PanicErrorImpl> for ErrorOrReturn {
    fn from(value: PanicErrorImpl) -> Self {
        ErrorOrReturn::Error(value.into())
    }
}

pub fn evaluate_function(
    func: &FunctionDecl,
    env: &mut Environment,
    decls: &HashMap<String, Decl>,
) -> Result<Value, PanicLangError> {
    let result = evaluate_statements(&func.stmts, env, decls);
    if let Err(error_or_return) = result {
        return match error_or_return {
            ErrorOrReturn::Error(err) => Err(err),
            ErrorOrReturn::Return(value) => Ok(value),
        };
    }
    Err(PanicErrorImpl::EvaluationError(format!(
        "function {} did not return a value at {:?}",
        func.ident.name, func.span,
    ))
    .into())
}

pub fn evaluate_statements(
    stmts: &[Stmt],
    env: &mut Environment,
    decls: &HashMap<String, Decl>,
) -> Result<(), ErrorOrReturn> {
    for stmt in stmts {
        evaluate_statement(stmt, env, decls)?;
    }
    Ok(())
}

pub fn evaluate_statement(
    stmt: &Stmt,
    env: &mut Environment,
    decls: &HashMap<String, Decl>,
) -> Result<(), ErrorOrReturn> {
    match stmt {
        Stmt::Empty(_) => {}
        Stmt::Let(let_stmt) => {
            let value = evaluate_expression(&let_stmt.expr, env, decls)?;
            env.set(let_stmt.ident.name.clone(), value);
        }
        Stmt::Return(return_stmt) => {
            let result = evaluate_expression(&return_stmt.expr, env, decls)?;
            return Err(ErrorOrReturn::Return(result));
        }
        Stmt::If(if_stmt) => {
            evaluate_if_statement(if_stmt, env, decls)?;
        }
        Stmt::Expr(expr_stmt) => {
            evaluate_expression(&expr_stmt.expr, env, decls)?;
        }
    }
    Ok(())
}

pub fn evaluate_if_statement(
    if_stmt: &IfStmt,
    env: &mut Environment,
    decls: &HashMap<String, Decl>,
) -> Result<(), ErrorOrReturn> {
    let conditional = evaluate_expression(&if_stmt.test, env, decls)?;
    if let Value::Bool(condition) = conditional {
        if condition {
            evaluate_statements(&if_stmt.then_statements, env, decls)?;
        } else {
            match &if_stmt.else_statements {
                Else::ElseIf(if_stmt) => {
                    evaluate_if_statement(if_stmt, env, decls)?;
                }
                Else::ElseStatements(stmts) => {
                    evaluate_statements(stmts, env, decls)?;
                }
                Else::Empty() => {
                    // do nothing
                }
            }
        }
    } else {
        return Err(PanicErrorImpl::EvaluationError(format!(
            "if condition value {:?} is not boolean at {:?}",
            conditional, if_stmt.test.span,
        ))
        .into());
    }
    Ok(())
}

pub fn evaluate_expression(
    expr: &Expr,
    env: &Environment,
    decls: &HashMap<String, Decl>,
) -> Result<Value, ErrorOrReturn> {
    let value = match &expr.expr {
        ExprType::IntLiteral(bigval) => match bigval.to_i32() {
            Some(val) => Value::Int32(val),
            None => Value::ArithmeticOverflow(expr.span),
        },
        ExprType::BoolLiteral(val) => Value::Bool(*val),
        ExprType::VarReference(var_ref) => env
            .get(&var_ref.name, var_ref.span)
            .map_err(ErrorOrReturn::Error)?,
        ExprType::FuncCall(iden, params) => {
            let func = decls.get(&iden.name);
            let func = match func {
                Some(decl) => decl,
                None => {
                    return Err(PanicErrorImpl::EvaluationError(format!(
                        "cannot find function {} at {:?}",
                        iden.name, iden.span
                    ))
                    .into())
                }
            };
            let Decl::Func(func) = func;
            if func.params.len() != params.len() {
                return Err(PanicErrorImpl::EvaluationError(format!(
                    "expected {} parameters and received {} parameters at {:?}",
                    func.params.len(),
                    params.len(),
                    iden.span,
                ))
                .into());
            }
            let params = params
                .iter()
                .map(|e| evaluate_expression(e, env, decls))
                .collect::<Result<Vec<_>, _>>()?;
            let mut environment = Environment::new(Some(env));
            func.params
                .iter()
                .zip(params)
                .for_each(|(decl, val)| environment.set(decl.ident.name.clone(), val));
            evaluate_function(func, &mut environment, decls).map_err(ErrorOrReturn::Error)?
        }
        ExprType::Add(exprs) => {
            let values = exprs
                .iter()
                .map(|e| evaluate_expression(e, env, decls))
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
            let lhs = evaluate_expression(lhs_expr, env, decls)?;
            let rhs = evaluate_expression(rhs_expr, env, decls)?;
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
                .map(|e| evaluate_expression(e, env, decls))
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
            let lhs = evaluate_expression(lhs_expr, env, decls)?;
            let rhs = evaluate_expression(rhs_expr, env, decls)?;
            match (lhs, rhs) {
                (Value::Int32(lhs), Value::Int32(rhs)) => {
                    if rhs == 0 {
                        Value::ArithmeticDivisionByZero(expr.span)
                    } else if let Some(result) = lhs.checked_div(rhs) {
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
            let lhs = evaluate_expression(lhs_expr, env, decls)?;
            let rhs = evaluate_expression(rhs_expr, env, decls)?;
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
            let lhs = evaluate_expression(lhs_expr, env, decls)?;
            let rhs = evaluate_expression(rhs_expr, env, decls)?;
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
            let lhs = evaluate_expression(lhs_expr, env, decls)?;
            let rhs = evaluate_expression(rhs_expr, env, decls)?;
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
            let lhs = evaluate_expression(lhs_expr, env, decls)?;
            let rhs = evaluate_expression(rhs_expr, env, decls)?;
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
            let lhs = evaluate_expression(lhs_expr, env, decls)?;
            let rhs = evaluate_expression(rhs_expr, env, decls)?;
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
            let lhs = evaluate_expression(lhs_expr, env, decls)?;
            let rhs = evaluate_expression(rhs_expr, env, decls)?;
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
                .map(|e| evaluate_expression(e, env, decls))
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
                .map(|e| evaluate_expression(e, env, decls))
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
            let val = evaluate_expression(operand, env, decls)?;
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
            let val = evaluate_expression(operand, env, decls)?;
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
            let val = evaluate_expression(inner, env, decls)?;
            return Err(ErrorOrReturn::Return(val));
        }
        ExprType::Paren(inner) => evaluate_expression(inner, env, decls)?,
    };
    Ok(value)
}
