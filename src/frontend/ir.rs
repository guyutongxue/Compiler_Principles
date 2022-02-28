use koopa::ir::builder::{BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::layout::{InstList, Layout};
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Type, TypeKind, Value, ValueKind};
use std::borrow::BorrowMut;
use std::error::Error;

use crate::frontend::ast::Param;

use super::ast::{CompUnit, Decl, FuncDecl, InitVal, TypeSpec};
use super::consteval::Eval;
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
  pub fn new(program: &'a mut Program, func_ast: &FuncDecl) -> Result<Self, Box<dyn Error>> {
    let func_ir_name = format!("@{}", func_ast.ident);
    let func_ir_param: Vec<_> = func_ast
      .params
      .iter()
      .map(|param| match param {
        Param::Int(ident) => (Some(format!("@{}", ident)), Type::get_i32()),
        Param::Pointer(ident) => (
          Some(format!("@{}", ident)),
          Type::get_pointer(Type::get_i32()),
        ),
      })
      .collect();
    let func_ir_type = match func_ast.func_type {
      TypeSpec::Int => Type::get_i32(),
      TypeSpec::Void => Type::get_unit(),
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

    if func_ast.body.is_some() {
      // %entry basic block
      let entry = this.add_bb()?;
      this.bb = Some(entry);

      // Store parameters to local variable
      for (i, param) in func_ast.params.iter().enumerate() {
        let param_name = match param {
          Param::Int(ident) => ident.clone(),
          Param::Pointer(ident) => ident.clone(),
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

  for decl in ast {
    match decl {
      Decl::Func(decl) => {
        let name = &decl.ident;
        let mut context = GenerateContext::new(&mut program, &decl)?;

        if let Some(block) = &decl.body {
          // Function definition
          if !SymbolTable::insert_global_def(name, Symbol::Func(context.func)) {
            Err(CompileError(format!(
              "Redefinition of function {}",
              &decl.ident
            )))?;
          }
          for i in block.iter() {
            stmt::generate(i, &mut context)?;
          }
        } else {
          // Function declaration
          SymbolTable::insert_global_decl(name, Symbol::Func(context.func));
        }
      }
      Decl::Const(ty, decl) => {
        if ty == TypeSpec::Void {
          Err(CompileError("Cannot declare variable of type void".into()))?;
        }
        for i in decl {
          let name = i.name.clone();
          let val = i.init_val.eval(None).ok_or(CompileError(format!(
            "Global variable {} must be initialized with constant expression",
            &name
          )))?;
          if !SymbolTable::insert_global_def(&name, Symbol::Const(val)) {
            Err(CompileError(format!("Redefinition of variable {}", &name)))?;
          }
        }
      }
      Decl::Var(ty, decl) => {
        if ty == TypeSpec::Void {
          Err(CompileError("Cannot declare variable of type void".into()))?;
        }
        for i in decl {
          let name = i.name.clone();
          let value = match i.init_val {
            Some(init) => match init {
              InitVal::Simple(exp) => {
                let init_val = exp.eval(None).ok_or(CompileError(format!(
                  "Global variable {} must be initialized with constant expression",
                  &name
                )))?;
                program.new_value().integer(init_val)
              }
            },
            None => program.new_value().zero_init(Type::get_i32()),
          };
          let alloc = program.new_value().global_alloc(value);
          program
            .borrow_mut()
            .set_value_name(alloc, Some(format!("%{}", name)));
          if !SymbolTable::insert_global_def(&name, Symbol::Var(alloc)) {
            Err(CompileError(format!("Redefinition of variable {}", &name)))?;
          }
        }
      }
    }
  }

  for (_, fd) in program.funcs_mut().iter_mut() {
    add_extra_ret(fd);
  }

  Ok(program)
}

/// Add `ret` value for bbs not ends with `ret`
fn add_extra_ret(fd: &mut FunctionData) {
  let mut need_ret_bbs = vec![];
  for (bb, bbn) in fd.layout().bbs() {
    if let Some(inst) = bbn.insts().back_key() {
      let kind = fd.dfg().value(*inst).kind();
      if matches!(kind, ValueKind::Return(_))
        || matches!(kind, ValueKind::Jump(_))
        || matches!(kind, ValueKind::Branch(_))
      {
        continue;
      }
      need_ret_bbs.push(*bb);
    } else {
      need_ret_bbs.push(*bb);
    }
  }
  for bb in need_ret_bbs {
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
