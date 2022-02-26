use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder};
use koopa::ir::{FunctionData, Program, Type};
use std::error::Error;

use super::ast::{BlockItem, CompUnit};
#[allow(unused_imports)]
use super::error::{PushKeyError, UnimplementedError};
use super::{expr, stmt};

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

  let mut context = stmt::GenerateContext {
    program: &mut program,
    func: main,
    bb: entry,
  };
  for i in ast.func_def.block.iter() {
    stmt::generate(i, &mut context)?;
  }

  Ok(program)
}
