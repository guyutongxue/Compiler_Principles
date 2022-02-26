use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder};
use std::error::Error;

use super::ast::{BlockItem, ConstDecl, Stmt, Decl};
use super::consteval::Eval;
use super::error::CompileError;
use super::expr;
use super::symbol::{SYMBOLS, Symbol};

pub type GenerateContext<'a> = expr::GenerateContext<'a>;

pub fn generate(item: &BlockItem, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
  match item {
    BlockItem::Stmt(stmt) => stmt.generate(context),
    BlockItem::Decl(Decl::Const(decl)) => decl.generate(context),
  }
}

trait GenerateStmt {
  fn generate(&self, context: &mut GenerateContext) -> Result<(), Box<dyn Error>>;
}

impl GenerateStmt for Stmt {
  fn generate(&self, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
    let ret_expr = &self.exp;
    let ret_val = expr::generate(ret_expr, context)?;
    let ret = context.dfg().new_value().ret(Some(ret_val));
    context.add_inst(ret)?;
    Ok(())
  }
}

impl GenerateStmt for ConstDecl {
  fn generate(&self, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
    for i in self.iter() {
      let name = i.ident.clone();
      let val = i.init_val.eval().ok_or(CompileError(format!(
        "Constexpr variable {} must be initialized with constant expression",
        &name
      )))?;
      let mut table = SYMBOLS.lock().unwrap();
      table.insert(name, Symbol::ConstSymbol(val));
    }
    Ok(())
  }
}
