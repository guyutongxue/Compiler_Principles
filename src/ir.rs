use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{FunctionData, Program, Type};
use std::error::Error;

use crate::ast::CompUnit;

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
  // TODO: use map_err
  main_data
    .layout_mut()
    .bbs_mut()
    .push_key_back(entry)
    .unwrap();

  // `ret 0` in %entry
  let retval = main_data
    .dfg_mut()
    .new_value()
    .integer(ast.func_def.block.stmt.num);
  let ret = main_data.dfg_mut().new_value().ret(Some(retval));
  // TODO: use map_err
  main_data
    .layout_mut()
    .bb_mut(entry)
    .insts_mut()
    .push_key_back(ret)
    .unwrap();

  Ok(program)
}
