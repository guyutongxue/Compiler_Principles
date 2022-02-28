use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};
use koopa::ir::{BinaryOp, Type, Value, ValueKind};
use std::error::Error;

use super::ast::{
  AddExp, AddOp, EqExp, EqOp, LAndExp, LOrExp, LVal, MulExp, MulOp, PrimaryExp, RelExp, RelOp,
  UnaryExp, UnaryOp,
};
use super::consteval::{consteval, Eval};
use super::error::CompileError;
use super::ir::GenerateContext;
use super::symbol::{Symbol, SymbolTable};

#[allow(unused_imports)]
use super::error::UnimplementedError;

pub fn generate<EvalExp: Eval + GenerateValue>(
  exp: &EvalExp,
  context: &mut GenerateContext,
) -> Result<Value, Box<dyn Error>> {
  // consteval
  if let Some(value) = consteval(exp, context) {
    let result = context.dfg().new_value().integer(value);
    return Ok(result);
  }
  // runtime
  exp.generate_value(context)
}

pub trait GenerateValue {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>>;
}

enum ShortCircuitingOp {
  Or,
  And,
}

fn generate_with_short_circuiting<EvalExp1, EvalExp2>(
  context: &mut GenerateContext,
  lhs: &EvalExp1,
  op: ShortCircuitingOp,
  rhs: &EvalExp2,
) -> Result<Value, Box<dyn Error>>
where
  EvalExp1: GenerateValue,
  EvalExp2: GenerateValue,
{
  let zero = context.dfg().new_value().integer(0);
  let one = context.dfg().new_value().integer(1);

  let result = context.dfg().new_value().alloc(Type::get_i32());
  let init_value = match op {
    ShortCircuitingOp::Or => one,
    ShortCircuitingOp::And => zero,
  };
  let init_result = context.dfg().new_value().store(init_value, result);

  let lhs = lhs.generate_value(context)?;
  let branch_op = match op {
    ShortCircuitingOp::Or => BinaryOp::Eq,
    ShortCircuitingOp::And => BinaryOp::NotEq,
  };
  let lhs_op_zero = context.dfg().new_value().binary(branch_op, lhs, zero);
  context.add_inst(result)?;
  context.add_inst(init_result)?;
  context.add_inst(lhs_op_zero)?;

  let true_bb = context.add_bb()?;
  let end_bb = context.add_bb()?;
  let branch = context
    .dfg()
    .new_value()
    .branch(lhs_op_zero, true_bb, end_bb);
  context.switch_bb(branch, Some(true_bb))?;

  let rhs = rhs.generate_value(context)?;
  let rhs_neq_zero = context.dfg().new_value().binary(BinaryOp::NotEq, rhs, zero);
  let rhs_store = context.dfg().new_value().store(rhs, result);
  context.add_inst(rhs_neq_zero)?;
  context.add_inst(rhs_store)?;

  let jump = context.dfg().new_value().jump(end_bb);
  let load = context.dfg().new_value().load(result);
  context.switch_bb(jump, Some(end_bb))?;
  context.add_inst(load)?;

  Ok(load)
}

impl GenerateValue for LOrExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>> {
    match self {
      LOrExp::And(exp) => generate(exp.as_ref(), context),
      LOrExp::Or(lhs, rhs) => {
        generate_with_short_circuiting(context, lhs.as_ref(), ShortCircuitingOp::Or, rhs.as_ref())
      }
    }
  }
}

impl GenerateValue for LAndExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value, Box<dyn Error>> {
    match self {
      LAndExp::Eq(exp) => generate(exp.as_ref(), context),
      LAndExp::And(lhs, rhs) => {
        generate_with_short_circuiting(context, lhs.as_ref(), ShortCircuitingOp::And, rhs.as_ref())
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
      UnaryExp::Call(func_name, args) => {
        let func = SymbolTable::get_global(func_name)
          .ok_or(CompileError(format!("Function {} undefined", func_name)))?;

        if let Symbol::Func(func) = func {
          let args = args
            .iter()
            .map(|arg| generate(arg.as_ref(), context))
            .collect::<Result<Vec<_>, _>>()?;
          let result = context.dfg().new_value().call(func, args);
          context.add_inst(result)?;
          Ok(result)
        } else {
          Err(CompileError(format!("{} is not a function", func_name)))?
        }
      }
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
      LVal::Ident(name) => {
        let symbol = context.symbol.get(name);

        match symbol {
          None => global_var_generate_value(name, context),
          Some(symbol) => match symbol {
            Symbol::Const(value) => Ok(context.dfg().new_value().integer(value)),
            Symbol::Var(val) => match context.dfg().value(val).kind() {
              ValueKind::Alloc(_) => {
                let load = context.dfg().new_value().load(val);
                context.add_inst(load)?;
                Ok(load)
              }
              ValueKind::FuncArgRef(_) => Ok(val),
              _ => Err(CompileError(format!("{} is not a variable", name)))?,
            },
            Symbol::Func(_) => Err(CompileError(format!(
              "Cannot use function as a value: {}",
              name
            )))?,
          },
        }
      }
    }
  }
}

fn global_var_generate_value(
  name: &str,
  context: &mut GenerateContext,
) -> Result<Value, Box<dyn Error>> {
  let symbol =
    SymbolTable::get_global(name).ok_or(CompileError(format!("Variable {} undefined", name)))?;
  match symbol {
    Symbol::Const(value) => Ok(context.dfg().new_value().integer(value)),
    Symbol::Var(val) => {
      let load = context.dfg().new_value().load(val);
      context.add_inst(load)?;
      Ok(load)
    }
    Symbol::Func(_) => Err(CompileError(format!(
      "Cannot use function as a value: {}",
      name
    )))?,
  }
}
