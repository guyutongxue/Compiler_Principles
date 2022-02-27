use koopa::ir::builder::BasicBlockBuilder;
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::layout::{InstList, Layout};
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Type, Value};
use std::error::Error;

use super::ast::CompUnit;
#[allow(unused_imports)]
use super::error::{PushKeyError, UnimplementedError};
use super::stmt;
use super::symbol::SymbolTable;

pub struct GenerateContext<'a> {
  pub program: &'a mut Program,
  pub func: Function,
  pub bb: Option<BasicBlock>,
  pub symbol: SymbolTable,

  next_bb_no: Box<dyn Iterator<Item = i32>>,
}

impl<'a> GenerateContext<'a> {
  pub fn new(program: &'a mut Program, func_name: &str) -> Result<Self, Box<dyn Error>> {
    let main = program.new_func(FunctionData::with_param_names(
      format!("@{}", func_name),
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

    Ok(Self {
      program: program,
      func: main,
      bb: Some(entry),
      symbol: SymbolTable::new(),
      next_bb_no: Box::new(0..),
    })
  }

  pub fn dfg(&mut self) -> &mut DataFlowGraph {
    self.program.func_mut(self.func).dfg_mut()
  }
  fn layout(&mut self) -> &mut Layout {
    self.program.func_mut(self.func).layout_mut()
  }

  pub fn add_bb(&mut self) -> Result<BasicBlock, Box<dyn Error>> {
    let name = format!("%bb{}", self.next_bb_no.next().unwrap());
    let bb = self.dfg().new_bb().basic_block(Some(name));
    self
      .layout()
      .bbs_mut()
      .push_key_back(bb)
      .map_err(|k| PushKeyError(Box::new(k)))?;
    Ok(bb)
  }

  pub fn insts(&mut self, bb: BasicBlock) -> &mut InstList {
    self.layout().bb_mut(bb).insts_mut()
  }

  pub fn add_inst(&mut self, value: Value) -> Result<(), Box<dyn Error>> {
    if let Some(bb) = self.bb {
      self
        .insts(bb)
        .push_key_back(value)
        .map_err(|k| {
          let vd = self.dfg().value(k).clone();
          PushKeyError(Box::new(vd))
        })?;
    }
    Ok(())
  }

  pub fn switch_bb(
    &mut self,
    final_inst: Value,
    new_bb: Option<BasicBlock>,
  ) -> Result<(), Box<dyn Error>> {
    self.add_inst(final_inst)?;
    self.bb = new_bb;
    Ok(())
  }
}

pub fn generate_program(ast: CompUnit) -> Result<Program, Box<dyn Error>> {
  let mut program = Program::new();

  let mut context = GenerateContext::new(&mut program, &ast.func_def.ident)?;

  for i in ast.func_def.block.iter() {
    stmt::generate(i, &mut context)?;
  }

  Ok(program)
}
