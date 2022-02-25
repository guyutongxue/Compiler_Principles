use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::layout::InstList;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value};
use std::error::Error;

use super::ast::{
  AddExp, AddOp, CompUnit, EqExp, Exp, LAndExp, LOrExp, MulExp, MulOp, PrimaryExp, RelExp,
  UnaryExp, UnaryOp,
};
use super::error::{PushKeyError, UnimplementedError};

struct GenerateValueContext<'a> {
  program: &'a mut Program,
  func: Function,
  bb: BasicBlock,
}

impl<'a> GenerateValueContext<'a> {
  fn dfg(&mut self) -> &mut DataFlowGraph {
    self.program.func_mut(self.func).dfg_mut()
  }
  fn insts(&mut self) -> &mut InstList {
    self
      .program
      .func_mut(self.func)
      .layout_mut()
      .bb_mut(self.bb)
      .insts_mut()
  }

  fn add_inst(&mut self, value: Value) -> Result<(), Box<dyn Error>> {
    self
      .insts()
      .push_key_back(value)
      .map_err(|k| PushKeyError(Box::new(k)))?;
    Ok(())
  }
}

pub fn generate_program(ast: CompUnit) -> Result<Program, Box<dyn Error>> {
  let mut program = Program::new();

  // @main Function
  let main = program.new_func(FunctionData::with_param_names(
    format!("@{}", ast.func_def.ident),
    vec![],
    Type::get_i32(),
  ));
  let main_data = program.func_mut(main);

  // %entry basic block
  let entry = main_data
    .dfg_mut()
    .new_bb()
    .basic_block(Some("%entry".into()));

  main_data
    .layout_mut()
    .bbs_mut()
    .push_key_back(entry)
    .map_err(|k| PushKeyError(Box::new(k)))?;

  // `ret ...` in %entry
  let mut context = GenerateValueContext {
    program: &mut program,
    func: main,
    bb: entry,
  };
  let retval = ast.func_def.block.stmt.exp.generate_value(&mut context)?;

  let main_data = program.func_mut(main);

  let ret = main_data.dfg_mut().new_value().ret(Some(retval));

  main_data
    .layout_mut()
    .bb_mut(entry)
    .insts_mut()
    .push_key_back(ret)
    .map_err(|k| PushKeyError(Box::new(k)))?;

  Ok(program)
}

trait GenerateValue {
  fn generate_value(&self, context: &mut GenerateValueContext) -> Result<Value, Box<dyn Error>>;
}

impl GenerateValue for Exp {
  fn generate_value(&self, context: &mut GenerateValueContext) -> Result<Value, Box<dyn Error>> {
    match self {
      Exp::LOr(exp) => exp.generate_value(context),
    }
  }
}

impl GenerateValue for LOrExp {
  fn generate_value(&self, context: &mut GenerateValueContext) -> Result<Value, Box<dyn Error>> {
    match self {
      LOrExp::And(exp) => exp.generate_value(context),
      x => Err(Box::new(UnimplementedError("LOrExp".into()))),
    }
  }
}

impl GenerateValue for LAndExp {
  fn generate_value(&self, context: &mut GenerateValueContext) -> Result<Value, Box<dyn Error>> {
    match self {
      LAndExp::Eq(exp) => exp.generate_value(context),
      x => Err(Box::new(UnimplementedError("LAndExp".into()))),
    }
  }
}

impl GenerateValue for EqExp {
  fn generate_value(&self, context: &mut GenerateValueContext) -> Result<Value, Box<dyn Error>> {
    match self {
      EqExp::Rel(exp) => exp.generate_value(context),
      x => Err(Box::new(UnimplementedError("EqExp".into()))),
    }
  }
}

impl GenerateValue for RelExp {
  fn generate_value(&self, context: &mut GenerateValueContext) -> Result<Value, Box<dyn Error>> {
    match self {
      RelExp::Add(exp) => exp.generate_value(context),
      x => Err(Box::new(UnimplementedError("RelExp".into()))),
    }
  }
}

impl GenerateValue for AddExp {
  fn generate_value(&self, context: &mut GenerateValueContext) -> Result<Value, Box<dyn Error>> {
    match self {
      AddExp::Mul(exp) => exp.generate_value(context),
      AddExp::Add(lhs, op, rhs) => {
        let lhs = lhs.generate_value(context)?;
        let rhs = rhs.generate_value(context)?;
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
  fn generate_value(&self, context: &mut GenerateValueContext) -> Result<Value, Box<dyn Error>> {
    match self {
      MulExp::Unary(exp) => exp.generate_value(context),
      MulExp::Mul(lhs, op, rhs) => {
        let lhs = lhs.generate_value(context)?;
        let rhs = rhs.generate_value(context)?;
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
  fn generate_value(&self, context: &mut GenerateValueContext) -> Result<Value, Box<dyn Error>> {
    match self {
      UnaryExp::Primary(exp) => match exp {
        PrimaryExp::Num(num) => {
          let value = context.dfg().new_value().integer(*num);
          Ok(value)
        }
        PrimaryExp::Paren(exp) => exp.generate_value(context),
      },
      UnaryExp::Op(op, exp) => match op {
        UnaryOp::Positive => exp.generate_value(context),
        UnaryOp::Negative => {
          let value = exp.generate_value(context)?;
          let zero = context.dfg().new_value().integer(0);
          let result = context.dfg().new_value().binary(BinaryOp::Sub, zero, value);
          context.add_inst(result)?;
          Ok(result)
        }
        UnaryOp::Not => {
          let value = exp.generate_value(context)?;
          let zero = context.dfg().new_value().integer(0);
          let result = context.dfg().new_value().binary(BinaryOp::Eq, value, zero);
          context.add_inst(result)?;
          Ok(result)
        }
      },
    }
  }
}
