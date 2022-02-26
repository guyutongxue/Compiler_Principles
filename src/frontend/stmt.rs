use koopa::ir::builder::LocalInstBuilder;
use koopa::ir::Type;
use std::error::Error;


use super::ast::{BlockItem, ConstDecl, Decl, InitVal, LVal, Stmt, VarDecl};
use super::consteval::Eval;
use super::expr;
use super::symbol::{Symbol, SYMBOLS};
use super::error::CompileError;

#[allow(unused_imports)]
use super::error::UnimplementedError;

pub type GenerateContext<'a> = expr::GenerateContext<'a>;

pub fn generate(item: &BlockItem, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
  match item {
    BlockItem::Stmt(stmt) => stmt.generate(context),
    BlockItem::Decl(decl) => decl.generate(context),
  }
}

trait GenerateStmt {
  fn generate(&self, context: &mut GenerateContext) -> Result<(), Box<dyn Error>>;
}

impl GenerateStmt for Stmt {
  fn generate(&self, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
    match self {
      Stmt::Return(exp) => {
        let ret_val = expr::generate(exp.as_ref(), context)?;
        let ret = context.dfg().new_value().ret(Some(ret_val));
        context.add_inst(ret)?;
      }
      Stmt::Assign(lval, exp) => {
        let table = SYMBOLS.read().unwrap();
        match lval {
          LVal::Ident(ident) => {
            let symbol = table
              .get(ident)
              .ok_or(CompileError(format!("Undefined variable: {}", ident)))?;
            match symbol {
              Symbol::Const(_) => Err(CompileError(format!(
                "Cannot assign to constant: {}",
                ident
              )))?,
              Symbol::Var(alloc) => {
                let exp = expr::generate(exp.as_ref(), context)?;
                let store = context.dfg().new_value().store(exp, *alloc);
                context.add_inst(store)?;
              }
            }
          }
        }
      }
    }
    Ok(())
  }
}

impl GenerateStmt for Decl {
  fn generate(&self, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
    match self {
      Decl::Const(constant) => constant.generate(context),
      Decl::Var(variable) => variable.generate(context),
    }
  }
}

impl GenerateStmt for ConstDecl {
  fn generate(&self, _: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
    for i in self.iter() {
      let name = i.ident.clone();
      let val = i.init_val.eval().ok_or(CompileError(format!(
        "Constexpr variable {} must be initialized with constant expression",
        &name
      )))?;
      let mut table = SYMBOLS.write().unwrap();
      table.insert(name, Symbol::Const(val));
    }
    Ok(())
  }
}

impl GenerateStmt for VarDecl {
  fn generate(&self, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
    for i in self.iter() {
      let name = i.ident.clone();
      let alloc = context.dfg().new_value().alloc(Type::get_i32());
      context.add_inst(alloc)?;
      if let Some(ref init) = i.init_val {
        match init {
          InitVal::Simple(exp) => {
            let init_val = expr::generate(exp.as_ref(), context)?;
            let store = context.dfg().new_value().store(init_val, alloc);
            context.add_inst(store)?;
          }
        }
      }
      let mut table = SYMBOLS.write().unwrap();
      table.insert(name, Symbol::Var(alloc));
    }
    Ok(())
  }
}
