use koopa::ir::builder::{BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::layout::{InstList, Layout};
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Type, TypeKind, Value, ValueKind};
use std::borrow::BorrowMut;

use super::ast::{CompUnit, Decl, Declarator, FuncDecl, InitializerLike, TypeSpec};
use super::error::CompileError;
#[allow(unused_imports)]
use super::error::{PushKeyError, UnimplementedError};
use super::stmt::{self, get_layout};
use super::symbol::ConstValue;
use super::symbol::{Symbol, SymbolTable};
use crate::frontend::expr::ty::SysyType;
use crate::Result;

pub struct GenerateContext<'a> {
  pub program: &'a mut Program,
  pub func: Function,
  pub bb: Option<BasicBlock>,
  pub symbol: SymbolTable,

  next_bb_no: i32,

  /// 循环中 break/continue 跳转位置
  pub loop_jump_pt: Vec<(BasicBlock, BasicBlock)>,
}

fn generate_param_list(params: &Vec<Box<Declarator>>) -> Result<Vec<(Option<String>, Type)>> {
  let mut ir = vec![];
  for param in params {
    let (tys, name) = SysyType::parse(param.as_ref(), None)?;
    let mut ir_ty = tys.to_ir();
    // Perform array-to-pointer conversion
    if let TypeKind::Array(ty, _) = ir_ty.kind() {
      ir_ty = Type::get_pointer(ty.clone());
    }
    ir.push((Some(format!("@{}", name)), ir_ty));
  }
  Ok(ir)
}

impl<'a> GenerateContext<'a> {
  pub fn new(program: &'a mut Program, func_ast: &FuncDecl) -> Result<Self> {
    let func_ir_name = format!("@{}", func_ast.ident);
    let func_ir_param = generate_param_list(&func_ast.params)?;
    let func_ir_type = match func_ast.func_type {
      TypeSpec::Int => Type::get_i32(),
      TypeSpec::Void => Type::get_unit(),
    };

    // Koopa IR 不允许重复声明函数。移除之前的声明。如果函数已有定义，则在符号表插入阶段报错。
    let func = if let Some((&f, _)) = program
      .funcs()
      .iter()
      .find(|(_, fd)| fd.name() == func_ir_name)
    {
      f
    } else {
      program.new_func(FunctionData::with_param_names(
        func_ir_name,
        func_ir_param,
        func_ir_type,
      ))
    };

    let mut this = Self {
      program: program,
      func,
      bb: None,
      symbol: SymbolTable::new(),
      next_bb_no: 0,
      loop_jump_pt: vec![],
    };

    if func_ast.body.is_some() {
      // %entry basic block
      let entry = this.add_bb("entry")?;
      this.bb = Some(entry);

      // Store parameters to local variable
      for (i, param) in func_ast.params.iter().enumerate() {
        let (tys, name) = SysyType::parse(param.as_ref(), None)?;
        let param = this.program.func(this.func).params()[i];
        let param_type = this.dfg().value(param).ty().clone();

        let alloc = this.dfg().new_value().alloc(param_type);
        let store = this.dfg().new_value().store(param, alloc);

        this.add_inst(alloc)?;
        this.dfg().set_value_name(alloc, Some(format!("%{}", name)));
        this.add_inst(store)?;

        if !this.symbol.insert(&name, Symbol::Var(tys, alloc)) {
          Err(CompileError::Redefinition(name.into()))?;
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

  pub fn new_bb_set(&mut self) {
    self.next_bb_no = self.next_bb_no + 1;
  }

  pub fn add_bb(&mut self, name: &str) -> Result<BasicBlock> {
    let name = format!("%bb_{}_{}", name, self.next_bb_no);
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

  pub fn value_ty_kind(&self, value: Value) -> TypeKind {
    let val = self.program.func(self.func).dfg().values().get(&value);
    if let Some(val) = val {
      return val.ty().kind().clone();
    }
    self.program.borrow_value(value).ty().kind().clone()
  }

  pub fn add_inst(&mut self, value: Value) -> Result<()> {
    if self.bb.is_none() {
      self.new_bb_set();
      self.bb = Some(self.add_bb("unreachable")?);
    }
    self
      .insts(self.bb.unwrap())
      .push_key_back(value)
      .map_err(|k| {
        let vd = self.dfg().value(k).clone();
        PushKeyError(Box::new(vd))
      })?;
    Ok(())
  }

  pub fn switch_bb(&mut self, final_inst: Value, new_bb: Option<BasicBlock>) -> Result<()> {
    self.add_inst(final_inst)?;
    self.bb = new_bb;
    Ok(())
  }
}

pub fn generate_program(ast: CompUnit) -> Result<Program> {
  // 参考 https://github.com/pku-minic/sysy-runtime-lib/blob/master/src/sysy.h
  let prelude = r#"
decl @getint(): i32
decl @getch(): i32
decl @getarray(*i32): i32
decl @putint(i32): i32
decl @putch(i32): i32
decl @putarray(i32, *i32): i32
decl @starttime(): i32
decl @stoptime(): i32
"#;
  let driver = koopa::front::Driver::from(prelude);
  let mut program = driver.generate_program().unwrap();
  for (func, func_data) in program.funcs() {
    let name = &func_data.name()[1..];
    if !SymbolTable::insert_global_def(name, Symbol::Func(*func)) {
      Err(CompileError::Redefinition(name.to_string()))?;
    }
  }

  for decl in &ast {
    match decl {
      Decl::Func(decl) => {
        let name = &decl.ident;
        let mut context = GenerateContext::new(&mut program, &decl)?;

        if let Some(block) = &decl.body {
          // Function definition
          if !SymbolTable::insert_global_def(name, Symbol::Func(context.func)) {
            Err(CompileError::Redefinition(decl.ident.clone()))?;
          }
          for i in block.iter() {
            stmt::generate(i, &mut context)?;
          }
        } else {
          // Function declaration
          SymbolTable::insert_global_decl(name, Symbol::Func(context.func));
        }
      }
      Decl::Var(declaration) => {
        if declaration.ty == TypeSpec::Void {
          Err(CompileError::IllegalVoid)?;
        }
        for (decl, init) in &declaration.list {
          let (tys, name) = SysyType::parse(decl.as_ref(), None)?;
          if declaration.is_const {
            // 全局常量声明
            let init = init
              .as_ref()
              .ok_or(CompileError::InitializerRequired(name.into()))?;
            // 对初始化器求值；若非常量表达式报错
            let const_value = match init.eval(None) {
              Err(e) => Err(e.to_compile_error("全局常量初始化器"))?,
              Ok(exp) => match &exp {
                InitializerLike::Simple(exp) => ConstValue::int(*exp),
                InitializerLike::Aggregate(_) => {
                  let size = tys.get_array_size();
                  let layout = get_layout(&size, &exp, &mut || 0)?;
                  ConstValue::from(size, layout)
                }
              },
            };
            if !SymbolTable::insert_global_def(name, Symbol::Const(const_value)) {
              Err(CompileError::Redefinition(name.into()))?;
            }
          } else {
            // 全局变量声明
            let value = match init {
              // 对初始化器求值，转换为 IR
              Some(init) => match init.eval(None) {
                Err(e) => Err(e.to_compile_error("全局变量初始化器"))?,
                Ok(exp) => match &exp {
                  InitializerLike::Simple(int) => program.new_value().integer(*int),
                  InitializerLike::Aggregate(_) => {
                    let size = tys.get_array_size();
                    let layout = get_layout(&size, &exp, &mut || 0)?;
                    // println!("{:#?}", &layout);
                    let const_value = ConstValue::from(size, layout);
                    const_value.to_ir(&mut program)
                  }
                },
              },
              None => program.new_value().zero_init(tys.to_ir()),
            };
            let alloc = program.new_value().global_alloc(value);
            // https://gitlab.eduxiji.net/pku-minic/QA-2022s/-/issues/1
            let ir_name = format!("%{}", if name == "init" { "glb_var_init" } else { name });
            program.borrow_mut().set_value_name(alloc, Some(ir_name));
            if !SymbolTable::insert_global_def(&name, Symbol::Var(tys, alloc)) {
              Err(CompileError::Redefinition(name.into()))?;
            }
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

trait ToIr {
  fn to_ir(&self, program: &mut Program) -> Value;
}

impl ToIr for ConstValue {
  fn to_ir(&self, program: &mut Program) -> Value {
    if self.size.len() == 0 {
      return program.new_value().integer(self.data[0]);
    } else {
      let mut values = vec![];
      for i in 0..self.size[0] {
        values.push(self.item(i as i32).unwrap().to_ir(program));
      }
      program.new_value().aggregate(values)
    }
  }
}
