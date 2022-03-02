use std::rc::Rc;

use koopa::ir::{Value, Type};

use super::ast::{
  AddExp, AddOp, EqExp, EqOp, LAndExp, LOrExp, LVal, MulExp, MulOp, PrimaryExp, RelExp, RelOp,
  UnaryExp, UnaryOp, Initializer, InitializerLike,
};
use super::error::{CompileError};
use super::decl::GenerateContext;
use super::symbol::{Symbol, SymbolTable};

#[derive(Debug, Clone)]
pub struct ConstValue {
  pub data: Vec<i32>,

  /// 当以变量下标访问常量数组时，仍然需要生成数组的 IR
  pub value: Option<Value>,

  /// [] 表示变量，即零维数组
  pub size: Vec<usize>,
}

impl ConstValue {
  pub fn int(number: i32) -> Self {
    Self {
      data: vec![number],
      value: None,
      size: vec![],
    }
  }

  pub fn from(size: Vec<usize>, data: Vec<i32>) -> Self {
    Self {
      data,
      value: None,
      size,
    }
  }

  pub fn as_int(&self) -> std::result::Result<i32, CompileError> {
    if self.size.len() == 0 {
      Ok(self.data[0])
    } else {
      Err(CompileError::TypeMismatch("整数", "".into(), "数组"))?
    }
  }

  pub fn item(&self, index: i32) -> std::result::Result<Self, CompileError> {
    if self.size.len() == 0 { Err(CompileError::TypeMismatch("数组", "".into(), "变量"))? }
    if index < 0 || index as usize >= self.size[0] {
      Err(CompileError::IndexOutOfBounds(index, self.size[0]))?
    }
    let index = index as usize;
    let step = self.size[1..].iter().fold(1, |acc, &x| acc * x);
    let start_index = index * step;
    let end_index = start_index + step;
    Ok(Self::from(self.size[1..].into(), self.data[start_index..end_index].into()))
  }

  pub fn ir_type(&self) -> Type {
    fn ir_type_impl(size: &[usize]) -> Type {
      if size.len() == 0 {
        Type::get_i32()
      } else {
        Type::get_array(ir_type_impl(size[1..].as_ref()), size[0])
      }
    }
    ir_type_impl(self.size.as_ref())
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
