use koopa::ir::Value;

use super::ast::{
  AddExp, AddOp, EqExp, EqOp, LAndExp, LOrExp, LVal, MulExp, MulOp, PrimaryExp, RelExp, RelOp,
  UnaryExp, UnaryOp,
};
use super::error::{CompileError};
use super::ir::GenerateContext;
use super::symbol::{Symbol, SymbolTable};
use crate::Result;

#[derive(Debug, Clone)]
pub struct ConstValue {
  data: Vec<i32>,

  /// 当以变量下标访问常量数组时，仍然需要生成数组的 IR
  value: Option<Value>,

  /// [] 表示变量，即零维数组
  size: Vec<usize>,
}

impl ConstValue {
  fn int(number: i32) -> Self {
    Self {
      data: vec![number],
      value: None,
      size: vec![],
    }
  }

  pub fn as_int(&self) -> std::result::Result<i32, CompileError> {
    if self.size.len() == 0 {
      Ok(self.data[0])
    } else {
      Err(CompileError::TypeMismatch("整数", "".into(), "数组"))?
    }
  }
}

pub enum EvalError {
  NotConstexpr,
  CompileError(CompileError),
}

pub type EvalResult = std::result::Result<ConstValue, EvalError>;

impl From<CompileError> for EvalError {
  fn from(error: CompileError) -> Self {
    EvalError::CompileError(error)
  }
}

pub fn consteval<EvalExp: Eval>(exp: &EvalExp, context: &GenerateContext) -> EvalResult {
  exp.eval(Some(context))
}

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
      UnaryExp::Call(_, _) => Err(EvalError::NotConstexpr)?,
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
          Some(context) => context.symbol.get(ident),
          None => SymbolTable::get_global(ident),
        };
        match symbol {
          Some(symbol) => match symbol {
            Symbol::Const(i) => Ok(i.clone()),
            Symbol::Var(_) => Err(EvalError::NotConstexpr)?,
            Symbol::Func(_) => Err(CompileError::TypeMismatch("变量", ident.clone(), "函数"))?,
          },
          None => Err(CompileError::UndeclaredSymbol(ident.clone()))?,
        }
      }
    }
  }
}
