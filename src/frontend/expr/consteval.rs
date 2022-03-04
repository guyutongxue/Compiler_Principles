use std::rc::Rc;

use super::GenerateContext;
use crate::frontend::ast::{
  AddExp, AddOp, AssignExp, EqExp, EqOp, Exp, Initializer, InitializerLike, LAndExp, LOrExp,
  MulExp, MulOp, PostfixExp, PrimaryExp, RelExp, RelOp, UnaryExp, UnaryOp,
};
use crate::frontend::error::CompileError;
use crate::frontend::symbol::{ConstValue, Symbol, SymbolTable};

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

impl Eval for Exp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      Exp::Assign(exp) => exp.eval(context),
      Exp::Comma(..) => Err(EvalError::NotConstexpr),
    }
  }
}

impl Eval for AssignExp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      AssignExp::LOr(exp) => exp.eval(context),
      AssignExp::Assign(_, rhs) => match context {
        None => rhs.eval(None),
        Some(_) => Err(EvalError::NotConstexpr),
      }
    }
  }
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
      UnaryExp::Postfix(exp) => exp.eval(context),
      UnaryExp::Op(op, exp) => {
        let exp = exp.eval(context)?.as_int()?;
        let result = match op {
          UnaryOp::Positive => exp,
          UnaryOp::Negative => -exp,
          UnaryOp::Not => (exp == 0) as i32,
        };
        Ok(ConstValue::int(result))
      }
      UnaryExp::Deref(_) => Err(EvalError::NotConstexpr),
      &UnaryExp::Address(_) => Err(EvalError::NotConstexpr),
    }
  }
}

impl Eval for PostfixExp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      PostfixExp::Primary(exp) => exp.eval(context),
      PostfixExp::Call(..) => Err(EvalError::NotConstexpr),
      PostfixExp::Subscript(lval, exp) => {
        let exp = exp.eval(context)?.as_int()?;
        let lval = lval.eval(context)?;
        lval.item(exp).map_err(|e| EvalError::CompileError(e))
      }
    }
  }
}

impl Eval for PrimaryExp {
  fn eval(&self, context: Option<&GenerateContext>) -> EvalResult {
    match self {
      PrimaryExp::Ident(ident) => {
        let symbol = match context {
          Some(context) => context
            .symbol
            .get(ident)
            .or_else(|| SymbolTable::get_global(ident)),
          None => SymbolTable::get_global(ident),
        };
        match symbol {
          Some(symbol) => match symbol {
            Symbol::Const(i) => Ok(i.clone()),
            Symbol::Var(..) => Err(EvalError::NotConstexpr)?,
            Symbol::Func(..) => Err(CompileError::TypeMismatch("变量", ident.clone(), "函数"))?,
          },
          None => Err(CompileError::UndeclaredSymbol(ident.clone()))?,
        }
      }
      PrimaryExp::Num(i) => Ok(ConstValue::int(*i)),
      PrimaryExp::Paren(exp) => exp.eval(context),
    }
  }
}

impl Initializer {
  pub fn eval(
    &self,
    context: Option<&GenerateContext>,
  ) -> std::result::Result<InitializerLike<i32>, EvalError> {
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
