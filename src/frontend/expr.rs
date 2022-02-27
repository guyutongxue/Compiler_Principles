use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::layout::InstList;
use koopa::ir::{BasicBlock, BinaryOp, Function, Program, Value};
use std::error::Error;

use super::consteval::Eval;
use super::error::CompileError;
use super::error::PushKeyError;

#[allow(unused_imports)]
use super::error::UnimplementedError;

use super::ast::{
  AddExp, AddOp, EqExp, EqOp, LAndExp, LOrExp, LVal, MulExp, MulOp, PrimaryExp, RelExp, RelOp,
  UnaryExp, UnaryOp,
};
use super::symbol::{Symbol, SymbolTable};

pub struct GenerateContext<'a> {
  pub program: &'a mut Program,
  pub func: Function,
  pub bb: BasicBlock,
  pub symbol: SymbolTable,
}

impl<'a> GenerateContext<'a> {
  pub fn dfg(&mut self) -> &mut DataFlowGraph {
    self.program.func_mut(self.func).dfg_mut()
  }
  pub fn insts(&mut self) -> &mut InstList {
    self
      .program
      .func_mut(self.func)
      .layout_mut()
      .bb_mut(self.bb)
      .insts_mut()
  }

  pub fn add_inst(&mut self, value: Value) -> Result<(), Box<dyn Error>> {
    self
      .insts()
      .push_key_back(value)
      .map_err(|k| PushKeyError(Box::new(k)))?;
    Ok(())
  }
}

pub fn generate<EvalExp: Eval + GenerateValue>(
  exp: &EvalExp,
  context: &mut GenerateContext,
) -> Result<Value, Box<dyn Error>> {
  // consteval
  if let Some(value) = exp.eval(context) {
    let result = context.dfg().new_value().integer(value);
    return Ok(result);
  }
  // runtime
  exp.generate_value(context)
}

pub trait GenerateValue {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>>;
}

impl GenerateValue for LOrExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>> {
    match self {
      LOrExp::And(exp) => generate(exp.as_ref(), context),
      LOrExp::Or(lhs, rhs) => {
        let zero = context.dfg().new_value().integer(0);
        let lhs = generate(lhs.as_ref(), context)?;
        let rhs = generate(rhs.as_ref(), context)?;
        let result = context.dfg().new_value().binary(BinaryOp::Or, lhs, rhs);
        context.add_inst(result)?;
        let result = context
          .dfg()
          .new_value()
          .binary(BinaryOp::NotEq, result, zero);
        context.add_inst(result)?;
        Ok(result)
      }
    }
  }
}

impl GenerateValue for LAndExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>> {
    match self {
      LAndExp::Eq(exp) => generate(exp.as_ref(), context),
      LAndExp::And(lhs, rhs) => {
        let zero = context.dfg().new_value().integer(0);
        let lhs = generate(lhs.as_ref(), context)?;
        let lhs = context.dfg().new_value().binary(BinaryOp::NotEq, lhs, zero);
        context.add_inst(lhs)?;
        let rhs = generate(rhs.as_ref(), context)?;
        let rhs = context.dfg().new_value().binary(BinaryOp::NotEq, rhs, zero);
        context.add_inst(rhs)?;
        let result = context.dfg().new_value().binary(BinaryOp::And, lhs, rhs);
        context.add_inst(result)?;
        Ok(result)
      }
    }
  }
}

impl GenerateValue for EqExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>> {
    match self {
      EqExp::Rel(exp) => generate(exp.as_ref(), context),
      EqExp::Eq(lhs, op, rhs) => {
        let lhs = generate(lhs.as_ref(), context)?;
        let rhs = generate(rhs.as_ref(), context)?;
        let op = match op {
          EqOp::Equal => BinaryOp::Eq,
          EqOp::NotEqual => BinaryOp::NotEq,
        };
        let result = context.dfg().new_value().binary(op, lhs, rhs);
        context.add_inst(result)?;
        Ok(result)
      }
    }
  }
}

impl GenerateValue for RelExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>> {
    match self {
      RelExp::Add(exp) => generate(exp.as_ref(), context),
      RelExp::Rel(lhs, op, rhs) => {
        let lhs = generate(lhs.as_ref(), context)?;
        let rhs = generate(rhs.as_ref(), context)?;
        let op = match op {
          RelOp::Less => BinaryOp::Lt,
          RelOp::LessEqual => BinaryOp::Le,
          RelOp::Greater => BinaryOp::Gt,
          RelOp::GreaterEqual => BinaryOp::Ge,
        };
        let result = context.dfg().new_value().binary(op, lhs, rhs);
        context.add_inst(result)?;
        Ok(result)
      }
    }
  }
}

impl GenerateValue for AddExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>> {
    match self {
      AddExp::Mul(exp) => generate(exp.as_ref(), context),
      AddExp::Add(lhs, op, rhs) => {
        let lhs = generate(lhs.as_ref(), context)?;
        let rhs = generate(rhs.as_ref(), context)?;
        let op = match op {
          AddOp::Plus => BinaryOp::Add,
          AddOp::Minus => BinaryOp::Sub,
        };
        let result = context.dfg().new_value().binary(op, lhs, rhs);
        context.add_inst(result)?;
        Ok(result)
      }
    }
  }
}

impl GenerateValue for MulExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>> {
    match self {
      MulExp::Unary(exp) => generate(exp.as_ref(), context),
      MulExp::Mul(lhs, op, rhs) => {
        let lhs = generate(lhs.as_ref(), context)?;
        let rhs = generate(rhs.as_ref(), context)?;
        let op = match op {
          MulOp::Multiply => BinaryOp::Mul,
          MulOp::Divide => BinaryOp::Div,
          &MulOp::Modulo => BinaryOp::Mod,
        };
        let result = context.dfg().new_value().binary(op, lhs, rhs);
        context.add_inst(result)?;
        Ok(result)
      }
    }
  }
}

impl GenerateValue for UnaryExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>> {
    match self {
      UnaryExp::Primary(exp) => exp.generate_value(context),
      UnaryExp::Op(op, exp) => match op {
        UnaryOp::Positive => generate(exp.as_ref(), context),
        UnaryOp::Negative => {
          let value = generate(exp.as_ref(), context)?;
          let zero = context.dfg().new_value().integer(0);
          let result = context.dfg().new_value().binary(BinaryOp::Sub, zero, value);
          context.add_inst(result)?;
          Ok(result)
        }
        UnaryOp::Not => {
          let value = generate(exp.as_ref(), context)?;
          let zero = context.dfg().new_value().integer(0);
          let result = context.dfg().new_value().binary(BinaryOp::Eq, value, zero);
          context.add_inst(result)?;
          Ok(result)
        }
      },
    }
  }
}

impl GenerateValue for PrimaryExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>> {
    match self {
      PrimaryExp::Paren(exp) => generate(exp.as_ref(), context),
      PrimaryExp::Num(num) => {
        let value = context.dfg().new_value().integer(*num);
        Ok(value)
      }
      PrimaryExp::LVal(lval) => lval.generate_value(context),
    }
  }
}

impl GenerateValue for LVal {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>> {
    match self {
      LVal::Ident(ident) => {
        let symbol = context
          .symbol
          .get(ident)
          .ok_or(CompileError(format!("Undefined variable: {}", ident)))?;
        match symbol {
          Symbol::Const(value) => Ok(context.dfg().new_value().integer(value)),
          Symbol::Var(alloc) => {
            let load = context.dfg().new_value().load(alloc);
            context.add_inst(load)?;
            Ok(load)
          }
        }
      }
    }
  }
}
