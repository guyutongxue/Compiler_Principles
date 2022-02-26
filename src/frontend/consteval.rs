use super::ast::{
  AddExp, AddOp, EqExp, EqOp, LAndExp, LOrExp, LVal, MulExp, MulOp, PrimaryExp, RelExp, RelOp,
  UnaryExp, UnaryOp,
};
use super::symbol::{SYMBOLS, Symbol};

pub trait Eval {
  fn eval(&self) -> Option<i32>;
}

impl Eval for LOrExp {
  fn eval(&self) -> Option<i32> {
    match self {
      LOrExp::And(exp) => exp.eval(),
      LOrExp::Or(lhs, rhs) => {
        let lhs = lhs.eval()? != 0;
        let rhs = rhs.eval()? != 0;
        Some((lhs || rhs) as i32)
      }
    }
  }
}

impl Eval for LAndExp {
  fn eval(&self) -> Option<i32> {
    match self {
      LAndExp::Eq(exp) => exp.eval(),
      LAndExp::And(lhs, rhs) => {
        let lhs = lhs.eval()? != 0;
        let rhs = rhs.eval()? != 0;
        Some((lhs && rhs) as i32)
      }
    }
  }
}

impl Eval for EqExp {
  fn eval(&self) -> Option<i32> {
    match self {
      EqExp::Rel(exp) => exp.eval(),
      EqExp::Eq(lhs, op, rhs) => {
        let lhs = lhs.eval()?;
        let rhs = rhs.eval()?;
        match op {
          EqOp::Equal => Some((lhs == rhs) as i32),
          EqOp::NotEqual => Some((lhs != rhs) as i32),
        }
      }
    }
  }
}

impl Eval for RelExp {
  fn eval(&self) -> Option<i32> {
    match self {
      RelExp::Add(exp) => exp.eval(),
      RelExp::Rel(lhs, op, rhs) => {
        let lhs = lhs.eval()?;
        let rhs = rhs.eval()?;
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
  fn eval(&self) -> Option<i32> {
    match self {
      AddExp::Mul(exp) => exp.eval(),
      AddExp::Add(lhs, op, rhs) => {
        let lhs = lhs.eval()?;
        let rhs = rhs.eval()?;
        match op {
          AddOp::Plus => Some(lhs + rhs),
          AddOp::Minus => Some(lhs - rhs),
        }
      }
    }
  }
}

impl Eval for MulExp {
  fn eval(&self) -> Option<i32> {
    match self {
      MulExp::Unary(exp) => exp.eval(),
      MulExp::Mul(lhs, op, rhs) => {
        let lhs = lhs.eval()?;
        let rhs = rhs.eval()?;
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
  fn eval(&self) -> Option<i32> {
    match self {
      UnaryExp::Primary(exp) => exp.eval(),
      UnaryExp::Op(op, exp) => {
        let exp = exp.eval()?;
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
  fn eval(&self) -> Option<i32> {
    match self {
      PrimaryExp::LVal(lval) => lval.eval(),
      PrimaryExp::Num(i) => Some(*i),
      PrimaryExp::Paren(exp) => exp.eval(),
    }
  }
}

impl Eval for LVal {
  fn eval(&self) -> Option<i32> {
    match self {
      LVal::Ident(ident) => {
        let table = SYMBOLS.lock().unwrap();
        let symbol = table.get(ident)?;
        match symbol {
          Symbol::ConstSymbol(i) => Some(*i),
        }
      }
    }
  }
}
