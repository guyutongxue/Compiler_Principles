use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};
use koopa::ir::{BinaryOp, Type, TypeKind, Value};

use super::ast::{
  AddExp, AddOp, EqExp, EqOp, LAndExp, LOrExp, LVal, MulExp, MulOp, PrimaryExp, RelExp, RelOp,
  UnaryExp, UnaryOp,
};
use super::consteval::{Eval, EvalError};
use super::error::CompileError;
use super::decl::GenerateContext;
use super::stmt::store_value_layout;
use super::symbol::{Symbol, SymbolTable};
use crate::Result;

#[allow(unused_imports)]
use super::error::UnimplementedError;

pub fn generate<EvalExp: Eval + GenerateValue>(
  exp: &EvalExp,
  context: &mut GenerateContext,
) -> Result<Value> {
  let eval_result = exp.eval(Some(context));
  match eval_result {
    Ok(cv) => {
      if let Ok(int) = cv.as_int() {
        // 如果常量表达式是整数，则直接生成整数 Value
        Ok(context.dfg().new_value().integer(int))
      } else {
        // 否则，意味着使用变量下标索引常量数组；
        // 必须将常量数组引入内存。
        let alloc = context.dfg().new_value().alloc(cv.ir_type());
        context.add_inst(alloc)?;
        let data: Vec<_> = cv
          .data
          .iter()
          .map(|&x| context.dfg().new_value().integer(x))
          .collect();
        store_value_layout(cv.size, alloc, data, context)?;
        Ok(alloc)
      }
    }
    Err(EvalError::NotConstexpr) => {
      return exp.generate_value(context);
    }
    Err(EvalError::CompileError(error)) => {
      return Err(error)?;
    }
  }
}

pub trait GenerateValue {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value>;
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
) -> Result<Value>
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
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      LOrExp::And(exp) => generate(exp.as_ref(), context),
      LOrExp::Or(lhs, rhs) => {
        generate_with_short_circuiting(context, lhs.as_ref(), ShortCircuitingOp::Or, rhs.as_ref())
      }
    }
  }
}

impl GenerateValue for LAndExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      LAndExp::Eq(exp) => generate(exp.as_ref(), context),
      LAndExp::And(lhs, rhs) => {
        generate_with_short_circuiting(context, lhs.as_ref(), ShortCircuitingOp::And, rhs.as_ref())
      }
    }
  }
}

impl GenerateValue for EqExp {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value> {
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
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value> {
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
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value> {
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
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value> {
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
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      UnaryExp::Primary(exp) => exp.generate_value(context),
      UnaryExp::Call(func_name, args) => {
        let func = SymbolTable::get_global(func_name)
          .ok_or(CompileError::UndeclaredSymbol(func_name.clone()))?;

        if let Symbol::Func(func) = func {
          let args = args
            .iter()
            .map(|arg| generate(arg.as_ref(), context))
            .collect::<Result<Vec<_>>>()?;
          let result = context.dfg().new_value().call(func, args);
          context.add_inst(result)?;
          Ok(result)
        } else {
          Err(CompileError::TypeMismatch(
            "函数",
            func_name.clone(),
            "变量/常量",
          ))?
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
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      PrimaryExp::Paren(exp) => generate(exp.as_ref(), context),
      PrimaryExp::Num(num) => {
        let value = context.dfg().new_value().integer(*num);
        Ok(value)
      }
      PrimaryExp::LVal(lval) => {
        let val = generate(lval, context)?;
        match context.value_ty_kind(val) {
          TypeKind::Pointer(base) => {
            match base.kind() {
              // Perform array-to-pointer conversion
              TypeKind::Array(..) => {
                let zero = context.dfg().new_value().integer(0);
                let elem = context.dfg().new_value().get_elem_ptr(val, zero);
                context.add_inst(elem)?;
                Ok(elem)
              }
              _ => {
                let load = context.dfg().new_value().load(val);
                context.add_inst(load)?;
                Ok(load)
              },
            }
          }
          TypeKind::Int32 => Ok(val),
          x => Err(CompileError::TypeMismatch("左值", x.to_string(), "其它"))?,
        }
      }
    }
  }
}

impl GenerateValue for LVal {
  fn generate_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      LVal::Ident(name) => {
        let symbol = context
          .symbol
          .get(name)
          .or_else(|| SymbolTable::get_global(name));
        // println!("{}: {:?}", name, symbol);
        match symbol {
          None => Err(CompileError::UndeclaredSymbol(name.into()))?,
          Some(symbol) => match symbol {
            Symbol::Const(_) => panic!("Constant Identifier: should unreachable"),
            Symbol::Var(_, val) => Ok(val),
            Symbol::Func(_) => Err(CompileError::TypeMismatch("变量", name.clone(), "函数"))?,
          },
        }
      }
      LVal::Subscript(lval, exp) => {
        let lval = generate(lval.as_ref(), context)?;
        let exp = generate(exp.as_ref(), context)?;
        // println!("SUB: {:?}", context.dfg().value(lval));
        match context.value_ty_kind(lval) {
          TypeKind::Pointer(base) => {
            let result = match base.kind() {
              TypeKind::Array(_, _) => context.dfg().new_value().get_elem_ptr(lval, exp),
              _ => {
                let load = context.dfg().new_value().load(lval);
                context.add_inst(load)?;
                context.dfg().new_value().get_ptr(load, exp)
              }
            };
            context.add_inst(result)?;
            Ok(result)
          }
          x => Err(CompileError::TypeMismatch(
            "可解地址的数组/指针",
            x.to_string(),
            "其它变量",
          ))?,
        }
      }
    }
  }
}
