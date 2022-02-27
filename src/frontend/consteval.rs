use super::ast::{
  AddExp, AddOp, EqExp, EqOp, LAndExp, LOrExp, LVal, MulExp, MulOp, PrimaryExp, RelExp, RelOp,
  UnaryExp, UnaryOp,
};
use super::ir::GenerateContext;
use super::symbol::Symbol;

pub trait Eval {
  fn eval(&self, context: &GenerateContext) -> Option<i32>;
}

impl Eval for LOrExp {
  fn eval(&self, context: &GenerateContext) -> Option<i32> {
    match self {
      LOrExp::And(exp) => exp.eval(context),
      LOrExp::Or(lhs, rhs) => {
        let lhs = lhs.eval(context)? != 0;
        let rhs = rhs.eval(context)? != 0;
        Some((lhs || rhs) as i32)
      }
    }
  }
}

impl Eval for LAndExp {
  fn eval(&self, context: &GenerateContext) -> Option<i32> {
    match self {
      LAndExp::Eq(exp) => exp.eval(context),
      LAndExp::And(lhs, rhs) => {
        let lhs = lhs.eval(context)? != 0;
        let rhs = rhs.eval(context)? != 0;
        Some((lhs && rhs) as i32)
      }
    }
  }
}

impl Eval for EqExp {
  fn eval(&self, context: &GenerateContext) -> Option<i32> {
    match self {
      EqExp::Rel(exp) => exp.eval(context),
      EqExp::Eq(lhs, op, rhs) => {
        let lhs = lhs.eval(context)?;
        let rhs = rhs.eval(context)?;
        match op {
          EqOp::Equal => Some((lhs == rhs) as i32),
          EqOp::NotEqual => Some((lhs != rhs) as i32),
        }
      }
    }
  }
}

impl Eval for RelExp {
  fn eval(&self, context: &GenerateContext) -> Option<i32> {
    match self {
      RelExp::Add(exp) => exp.eval(context),
      RelExp::Rel(lhs, op, rhs) => {
        let lhs = lhs.eval(context)?;
        let rhs = rhs.eval(context)?;
        match op {
          RelOp::Less => Some((lhs < rhs) as i32),
          RelOp::LessEqual => Some((lhs <= rhs) as i32),
          RelOp::Greater => Some((lhs > rhs) as i32),
          RelOp::GreaterEqual => Some((lhs >= rhs) as i32),
        }
      }
    }
  }
}

impl Eval for AddExp {
  fn eval(&self, context: &GenerateContext) -> Option<i32> {
    match self {
      AddExp::Mul(exp) => exp.eval(context),
      AddExp::Add(lhs, op, rhs) => {
        let lhs = lhs.eval(context)?;
        let rhs = rhs.eval(context)?;
        match op {
          AddOp::Plus => Some(lhs + rhs),
          AddOp::Minus => Some(lhs - rhs),
        }
      }
    }
  }
}

impl Eval for MulExp {
  fn eval(&self, context: &GenerateContext) -> Option<i32> {
    match self {
      MulExp::Unary(exp) => exp.eval(context),
      MulExp::Mul(lhs, op, rhs) => {
        let lhs = lhs.eval(context)?;
        let rhs = rhs.eval(context)?;
        match op {
          MulOp::Multiply => Some(lhs * rhs),
          MulOp::Divide => Some(lhs / rhs),
          MulOp::Modulo => Some(lhs % rhs),
        }
      }
    }
  }
}

impl Eval for UnaryExp {
  fn eval(&self, context: &GenerateContext) -> Option<i32> {
    match self {
      UnaryExp::Primary(exp) => exp.eval(context),
      UnaryExp::Op(op, exp) => {
        let exp = exp.eval(context)?;
        match op {
          UnaryOp::Positive => Some(exp),
          UnaryOp::Negative => Some(-exp),
          UnaryOp::Not => Some((exp == 0) as i32),
        }
      }
    }
  }
}

impl Eval for PrimaryExp {
  fn eval(&self, context: &GenerateContext) -> Option<i32> {
    match self {
      PrimaryExp::LVal(lval) => lval.eval(context),
      PrimaryExp::Num(i) => Some(*i),
      PrimaryExp::Paren(exp) => exp.eval(context),
    }
  }
}

impl Eval for LVal {
  fn eval(&self, context: &GenerateContext) -> Option<i32> {
    match self {
      LVal::Ident(ident) => {
        let symbol = context.symbol.get(ident)?;
        match symbol {
          Symbol::Const(i) => Some(i),
          Symbol::Var(_) => None,
        }
      }
    }
  }
}
