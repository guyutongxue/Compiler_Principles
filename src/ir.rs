use core::fmt;
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::layout::InstList;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value};
use std::error::Error;

use crate::ast::{CompUnit, Exp, PrimaryExp, UnaryExp, UnaryOp};

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
}

#[derive(Debug)]
struct PushKeyError(Box<dyn fmt::Debug>);

impl Error for PushKeyError {}

impl fmt::Display for PushKeyError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "key {:#?} already exists", self.0)
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
      Exp::Unary(exp) => exp.generate_value(context),
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
        UnaryOp::Pos => exp.generate_value(context),
        UnaryOp::Neg => {
          let value = exp.generate_value(context)?;
          let zero = context.dfg().new_value().integer(0);
          let neg = context.dfg().new_value().binary(BinaryOp::Sub, zero, value);
          context.insts().push_key_back(neg).map_err(|k| PushKeyError(Box::new(k)))?;
          Ok(neg)
        }
        UnaryOp::Not => {
          let value = exp.generate_value(context)?;
          let zero = context.dfg().new_value().integer(0);
          let not = context.dfg().new_value().binary(BinaryOp::Eq, value, zero);
          context.insts().push_key_back(not).map_err(|k| PushKeyError(Box::new(k)))?;
          Ok(not)
        }
      },
    }
  }
}
