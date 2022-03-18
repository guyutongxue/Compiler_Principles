pub mod category;
pub mod consteval;
pub mod ty;

use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};
use koopa::ir::{BinaryOp, Type, Value};

use self::category::Category;

use super::ast::{
  AddExp, AddOp, AssignExp, EqExp, EqOp, Exp, LAndExp, LOrExp, MulExp, MulOp, PostfixExp,
  PrimaryExp, RelExp, RelOp, UnaryExp, UnaryOp,
};
use super::decl::GenerateContext;
use super::error::CompileError;
use super::stmt::store_value_layout;
use super::symbol::{Symbol, SymbolTable};
use crate::Result;

use category::{GetCategory, ExpectCategory};
use consteval::{Eval, EvalError};
use ty::GetType;

#[allow(unused_imports)]
use super::error::UnimplementedError;

pub fn generate<EvalExp: ToIrValue>(
  exp: &EvalExp,
  context: &mut GenerateContext,
) -> Result<Value> {
  let eval_result = exp.eval(Some(context));
  exp.get_type(Some(context))?;
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
        store_value_layout(cv.ty.get_array_size(), alloc, data, context)?;
        Ok(alloc)
      }
    }
    Err(EvalError::NotConstexpr) => {
      return exp.to_ir_value(context);
    }
    Err(EvalError::CompileError(error)) => {
      return Err(error)?;
    }
  }
}

pub trait ToIrValue: Eval + GetType + GetCategory {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value>;
}

impl ToIrValue for Exp {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      Exp::Assign(exp) => generate(exp.as_ref(), context),
      Exp::Comma(lhs, rhs) => {
        let _lhs = generate(lhs.as_ref(), context)?;
        let rhs = generate(rhs.as_ref(), context)?;
        Ok(rhs)
      }
    }
  }
}

impl ToIrValue for AssignExp {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      AssignExp::LOr(exp) => generate(exp.as_ref(), context),
      AssignExp::Assign(lhs, rhs) => {
        let lhs = lhs.expect(Category::LValue)?.generate(context)?;
        let rhs = rhs.expect(Category::RValue)?.generate(context)?;
        // println!("ASSIGN-L: {:?}", context.dfg().value(lhs));
        // println!("ASSIGN-R: {:?}", context.dfg().value(rhs));
        let store = context.dfg().new_value().store(rhs, lhs);
        context.add_inst(store)?;
        Ok(lhs)
      }
    }
  }
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
  EvalExp1: ToIrValue,
  EvalExp2: ToIrValue,
{
  let zero = context.dfg().new_value().integer(0);
  let one = context.dfg().new_value().integer(1);

  let result = context.dfg().new_value().alloc(Type::get_i32());
  let init_value = match op {
    ShortCircuitingOp::Or => one,
    ShortCircuitingOp::And => zero,
  };
  let init_result = context.dfg().new_value().store(init_value, result);

  let lhs = lhs.expect(Category::RValue)?.generate(context)?;
  let branch_op = match op {
    ShortCircuitingOp::Or => BinaryOp::Eq,
    ShortCircuitingOp::And => BinaryOp::NotEq,
  };
  let lhs_op_zero = context.dfg().new_value().binary(branch_op, lhs, zero);
  context.add_inst(result)?;
  context.add_inst(init_result)?;
  context.add_inst(lhs_op_zero)?;

  context.new_bb_set();
  let true_bb = context.add_bb("sc_if_true")?;
  let end_bb = context.add_bb("sc_if_end")?;
  let branch = context
    .dfg()
    .new_value()
    .branch(lhs_op_zero, true_bb, end_bb);
  context.switch_bb(branch, Some(true_bb))?;

  let rhs = rhs.expect(Category::RValue)?.generate(context)?;
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

impl ToIrValue for LOrExp {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      LOrExp::And(exp) => generate(exp.as_ref(), context),
      LOrExp::Or(lhs, rhs) => {
        generate_with_short_circuiting(context, lhs.as_ref(), ShortCircuitingOp::Or, rhs.as_ref())
      }
    }
  }
}

impl ToIrValue for LAndExp {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      LAndExp::Eq(exp) => generate(exp.as_ref(), context),
      LAndExp::And(lhs, rhs) => {
        generate_with_short_circuiting(context, lhs.as_ref(), ShortCircuitingOp::And, rhs.as_ref())
      }
    }
  }
}

impl ToIrValue for EqExp {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      EqExp::Rel(exp) => generate(exp.as_ref(), context),
      EqExp::Eq(lhs, op, rhs) => {
        let lhs = lhs.expect(Category::RValue)?.generate(context)?;
        let rhs = rhs.expect(Category::RValue)?.generate(context)?;
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

impl ToIrValue for RelExp {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      RelExp::Add(exp) => generate(exp.as_ref(), context),
      RelExp::Rel(lhs, op, rhs) => {
        let lhs = lhs.expect(Category::RValue)?.generate(context)?;
        let rhs = rhs.expect(Category::RValue)?.generate(context)?;
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

impl ToIrValue for AddExp {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      AddExp::Mul(exp) => generate(exp.as_ref(), context),
      AddExp::Add(lhs, op, rhs) => {
        let lhs = lhs.expect(Category::RValue)?.generate(context)?;
        let rhs = rhs.expect(Category::RValue)?.generate(context)?;
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

impl ToIrValue for MulExp {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      MulExp::Unary(exp) => generate(exp.as_ref(), context),
      MulExp::Mul(lhs, op, rhs) => {
        let lhs = lhs.expect(Category::RValue)?.generate(context)?;
        let rhs = rhs.expect(Category::RValue)?.generate(context)?;
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

impl ToIrValue for UnaryExp {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      UnaryExp::Postfix(exp) => generate(exp.as_ref(), context),
      UnaryExp::Address(exp) => {
        exp.expect(Category::LValue)?.generate(context)
      }
      UnaryExp::Deref(exp) => {
        exp.expect(Category::RValue)?.generate(context)
      },
      UnaryExp::Op(op, exp) => match op {
        UnaryOp::Positive => exp.expect(Category::RValue)?.generate(context),
        UnaryOp::Negative => {
          let value = exp.expect(Category::RValue)?.generate(context)?;
          let zero = context.dfg().new_value().integer(0);
          let result = context.dfg().new_value().binary(BinaryOp::Sub, zero, value);
          context.add_inst(result)?;
          Ok(result)
        }
        UnaryOp::Not => {
          let value = exp.expect(Category::RValue)?.generate(context)?;
          let zero = context.dfg().new_value().integer(0);
          let result = context.dfg().new_value().binary(BinaryOp::Eq, value, zero);
          context.add_inst(result)?;
          Ok(result)
        }
      },
    }
  }
}

impl ToIrValue for PostfixExp {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      PostfixExp::Primary(exp) => generate(exp, context),
      PostfixExp::Call(func_name, args) => {
        let func = SymbolTable::get_global(func_name)
          .ok_or(CompileError::UndeclaredSymbol(func_name.clone()))?;

        if let Symbol::Func(_, func) = func {
          let args = args
            .iter()
            .map(|arg| arg.expect(Category::RValue)?.generate(context))
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
      PostfixExp::Subscript(lhs, rhs) => {
        let lhs = lhs.expect(Category::RValue)?.generate(context)?;
        let rhs = rhs.expect(Category::RValue)?.generate(context)?;
        // println!("SUB: {:?}", context.dfg().value(lhs));
        let result = context.dfg().new_value().get_ptr(lhs, rhs);
        context.add_inst(result)?;
        Ok(result)
      }
    }
  }
}

impl ToIrValue for PrimaryExp {
  fn to_ir_value(&self, context: &mut GenerateContext) -> Result<Value> {
    match self {
      PrimaryExp::Paren(exp) => generate(exp.as_ref(), context),
      PrimaryExp::Num(num) => {
        let value = context.dfg().new_value().integer(*num);
        Ok(value)
      }
      PrimaryExp::Ident(lval) => {
        let symbol = context
          .symbol
          .get(lval)
          .or_else(|| SymbolTable::get_global(lval));
        match symbol {
          None => Err(CompileError::UndeclaredSymbol(lval.into()))?,
          Some(symbol) => match symbol {
            Symbol::Const(_) => panic!("constant identifier: should unreachable"),
            Symbol::Var(_, val) => Ok(val),
            Symbol::Func(..) => Err(CompileError::TypeMismatch("变量", lval.clone(), "函数"))?,
          },
        }
      }
    }
  }
}
