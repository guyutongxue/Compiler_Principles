use std::rc::Rc;

use crate::frontend::ast::{
  AddExp, AddOp, EqExp, EqOp, LAndExp, LOrExp, LVal, MulExp, MulOp, PrimaryExp, RelExp, RelOp,
  UnaryExp, UnaryOp, Initializer, InitializerLike,
};
use crate::frontend::error::{CompileError};
use super::GenerateContext;
use crate::frontend::symbol::{Symbol, SymbolTable, ConstValue};

pub enum EvalError {
  NotConstexpr,
  CompileError(CompileError),
}

impl EvalError {
  pub fn to_compile_error(self, what: &'static str) -> CompileError {
    match self {
      EvalError::NotConstexpr => CompileError::ConstexprRequired(what),
      EvalError::CompileError(e) => e,
    }
  }
}

impl From<CompileError> for EvalError {
  fn from(error: CompileError) -> Self {
    EvalError::CompileError(error)
  }
}

pub type EvalResult = std::result::Result<ConstValue, EvalError>;

pub trait Eval {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult;
}

impl Eval for LOrExp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      LOrExp::And(exp) => exp.eval(context),
      LOrExp::Or(lhs, rhs) => {
        let lhs = lhs.eval(context)?.as_int()? != 0;
        let rhs = rhs.eval(context)?.as_int()? != 0;
        Ok(ConstValue::int((lhs || rhs) as i32))
      }
    }
  }
}

impl Eval for LAndExp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      LAndExp::Eq(exp) => exp.eval(context),
      LAndExp::And(lhs, rhs) => {
        let lhs = lhs.eval(context)?.as_int()? != 0;
        let rhs = rhs.eval(context)?.as_int()? != 0;
        Ok(ConstValue::int((lhs && rhs) as i32))
      }
    }
  }
}

impl Eval for EqExp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      EqExp::Rel(exp) => exp.eval(context),
      EqExp::Eq(lhs, op, rhs) => {
        let lhs = lhs.eval(context)?.as_int()?;
        let rhs = rhs.eval(context)?.as_int()?;
        let result = match op {
          EqOp::Equal => (lhs == rhs) as i32,
          EqOp::NotEqual => (lhs != rhs) as i32,
        };
        Ok(ConstValue::int(result))
      }
    }
  }
}

impl Eval for RelExp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      RelExp::Add(exp) => exp.eval(context),
      RelExp::Rel(lhs, op, rhs) => {
        let lhs = lhs.eval(context)?.as_int()?;
        let rhs = rhs.eval(context)?.as_int()?;
        let result = match op {
          RelOp::Less => (lhs < rhs) as i32,
          RelOp::LessEqual => (lhs <= rhs) as i32,
          RelOp::Greater => (lhs > rhs) as i32,
          RelOp::GreaterEqual => (lhs >= rhs) as i32,
        };
        Ok(ConstValue::int(result))
      }
    }
  }
}

impl Eval for AddExp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      AddExp::Mul(exp) => exp.eval(context),
      AddExp::Add(lhs, op, rhs) => {
        let lhs = lhs.eval(context)?.as_int()?;
        let rhs = rhs.eval(context)?.as_int()?;
        let result = match op {
          AddOp::Plus => lhs + rhs,
          AddOp::Minus => lhs - rhs,
        };
        Ok(ConstValue::int(result))
      }
    }
  }
}

impl Eval for MulExp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      MulExp::Unary(exp) => exp.eval(context),
      MulExp::Mul(lhs, op, rhs) => {
        let lhs = lhs.eval(context)?.as_int()?;
        let rhs = rhs.eval(context)?.as_int()?;
        let result = match op {
          MulOp::Multiply => lhs * rhs,
          MulOp::Divide => lhs / rhs,
          MulOp::Modulo => lhs % rhs,
        };
        Ok(ConstValue::int(result))
      }
    }
  }
}

impl Eval for UnaryExp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      UnaryExp::Primary(exp) => exp.eval(context),
      UnaryExp::Call(..) => Err(EvalError::NotConstexpr)?,
      UnaryExp::Op(op, exp) => {
        let exp = exp.eval(context)?.as_int()?;
        let result = match op {
          UnaryOp::Positive => exp,
          UnaryOp::Negative => -exp,
          UnaryOp::Not => (exp == 0) as i32,
        };
        Ok(ConstValue::int(result))
      }
    }
  }
}

impl Eval for PrimaryExp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      PrimaryExp::LVal(lval) => lval.eval(context),
      PrimaryExp::Address(_) => Err(EvalError::NotConstexpr)?,
      PrimaryExp::Num(i) => Ok(ConstValue::int(*i)),
      PrimaryExp::Paren(exp) => exp.eval(context),
    }
  }
}

impl Eval for LVal {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      LVal::Ident(ident) => {
        let symbol = match context {
          Some(context) => context.symbol.get(ident).or_else(|| SymbolTable::get_global(ident)),
          None => SymbolTable::get_global(ident),
        };
        match symbol {
          Some(symbol) => match symbol {
            Symbol::Const(i) => Ok(i.clone()),
            Symbol::Var(..) => Err(EvalError::NotConstexpr)?,
            Symbol::Func(_) => Err(CompileError::TypeMismatch("变量", ident.clone(), "函数"))?,
          },
          None => Err(CompileError::UndeclaredSymbol(ident.clone()))?,
        }
      },
      LVal::Deref(_) => Err(EvalError::NotConstexpr)?,
      LVal::Subscript(lval, exp) => {
        let exp = exp.eval(context)?.as_int()?;
        let lval = lval.eval(context)?;
        lval.item(exp).map_err(|e| EvalError::CompileError(e))
      },
    }
  }
}

impl Initializer {
  pub fn eval(&self, context: Option<&GenerateContext>) -> std::result::Result<InitializerLike<i32>, EvalError> {
    match self {
      Initializer::Simple(exp) => Ok(InitializerLike::Simple(exp.eval(context)?.as_int()?)),
      Initializer::Aggregate(aggr) => {
        let mut result: Vec<Rc<_>> = vec![];
        for exp in aggr {
          result.push(exp.eval(context)?.into());
        }
        Ok(InitializerLike::Aggregate(result))
      }
    }
  }
}
