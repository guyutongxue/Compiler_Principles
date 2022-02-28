use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::layout::{InstList, Layout};
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Type, TypeKind, Value};
use std::error::Error;

use crate::frontend::ast::Param;

use super::ast::{CompUnit, FuncDef, FuncType, GlobalDef};
use super::error::CompileError;
#[allow(unused_imports)]
use super::error::{PushKeyError, UnimplementedError};
use super::stmt;
use super::symbol::{Symbol, SymbolTable};

pub struct GenerateContext<'a> {
  pub program: &'a mut Program,
  pub func: Function,
  pub bb: Option<BasicBlock>,
  pub symbol: SymbolTable,

  next_bb_no: Box<dyn Iterator<Item = i32>>,

  /// 循环中 break/continue 跳转位置
  pub loop_jump_pt: Vec<(BasicBlock, BasicBlock)>,
}

impl<'a> GenerateContext<'a> {
  pub fn new(program: &'a mut Program, func_ast: &FuncDef) -> Result<Self, Box<dyn Error>> {
    let func_ir_name = format!("@{}", func_ast.ident);
    let func_ir_param: Vec<_> = func_ast
      .param_list
      .iter()
      .map(|param| match param {
        Param::Ident(ident) => (Some(format!("@{}", ident)), Type::get_i32()),
      })
      .collect();
    let func_ir_type = match func_ast.func_type {
      FuncType::Int => Type::get_i32(),
      FuncType::Void => Type::get_unit(),
    };

    let func = program.new_func(FunctionData::with_param_names(
      func_ir_name,
      func_ir_param,
      func_ir_type,
    ));

    let mut this = Self {
      program: program,
      func,
      bb: None,
      symbol: SymbolTable::new(),
      next_bb_no: Box::new(0..),
      loop_jump_pt: vec![],
    };

    // %entry basic block
    let entry = this.add_bb()?;
    this.bb = Some(entry);

    // Store parameters to local variable
    for (i, param) in func_ast.param_list.iter().enumerate() {
      let param_name = match param {
        Param::Ident(ident) => ident.clone(),
      };
      let param_type = Type::get_i32();
      let alloc = this.dfg().new_value().alloc(param_type);
      let param = this.program.func(this.func).params()[i];
      let store = this.dfg().new_value().store(param, alloc);

      this.add_inst(alloc)?;
      this.add_inst(store)?;

      if !this.symbol.insert(&param_name, Symbol::Var(alloc)) {
        Err(CompileError(format!(
          "Multiple parameters with a same name {}",
          &param_name
        )))?;
      }
    }
    Ok(this)
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
      self.insts(bb).push_key_back(value).map_err(|k| {
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

  for def in ast {
    match def {
      GlobalDef::Func(def) => {
        let mut context = GenerateContext::new(&mut program, &def)?;
        if !SymbolTable::insert_global(&def.ident, Symbol::Func(context.func)) {
          Err(CompileError(format!(
            "Redefinition of function {}",
            &def.ident
          )))?;
        }

        for i in def.block.iter() {
          stmt::generate(i, &mut context)?;
        }
      }
    }
  }

  for (_, fd) in program.funcs_mut().iter_mut() {
    add_return_for_empty_bb(fd);
  }

  Ok(program)
}

/// Add a return instruction for each empty bb.
/// Required for void functions.
fn add_return_for_empty_bb(fd: &mut FunctionData) {
  let empty_bbs: Vec<BasicBlock> = fd
    .layout()
    .bbs()
    .iter()
    .filter(|(_, bbn)| bbn.insts().len() == 0)
    .map(|(bb, _)| bb.clone())
    .collect();

  for bb in empty_bbs {
    if let TypeKind::Function(_, ret_type) = fd.ty().kind() {
      let ret = if Type::is_i32(ret_type) {
        let retval = fd.dfg_mut().new_value().integer(0);
        fd.dfg_mut().new_value().ret(Some(retval))
      } else {
        fd.dfg_mut().new_value().ret(None)
      };
      fd.layout_mut()
        .bb_mut(bb)
        .insts_mut()
        .push_key_back(ret)
        .unwrap();
    }
  }
}
