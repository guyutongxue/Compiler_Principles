use koopa::ir::builder::LocalInstBuilder;
use koopa::ir::Type;
use std::error::Error;

use super::ast::{BlockItem, ConstDecl, Decl, InitVal, LVal, Stmt, VarDecl};
use super::consteval::Eval;
use super::error::CompileError;
use super::expr;
use super::ir::GenerateContext;
use super::symbol::Symbol;

#[allow(unused_imports)]
use super::error::UnimplementedError;

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
      Stmt::Assign(lval, exp) => match lval {
        LVal::Ident(ident) => {
          let symbol = context
            .symbol
            .get(ident)
            .ok_or(CompileError(format!("Undefined variable: {}", ident)))?;
          match symbol {
            Symbol::Const(_) => Err(CompileError(format!(
              "Cannot assign to constant: {}",
              ident
            )))?,
            Symbol::Var(alloc) => {
              let exp = expr::generate(exp.as_ref(), context)?;
              let store = context.dfg().new_value().store(exp, alloc);
              context.add_inst(store)?;
            }
          }
        }
      },
      Stmt::Exp(exp) => {
        if let Some(exp) = exp {
          expr::generate(exp.as_ref(), context)?;
        }
      }
      Stmt::Block(block) => {
        context.symbol.push();
        for item in block.iter() {
          generate(item, context)?;
        }
        context.symbol.pop();
      }
      Stmt::If(exp, true_stmt, false_stmt) => {
        let cond = expr::generate(exp.as_ref(), context)?;
        let true_bb = context.add_bb()?;
        let end_bb = context.add_bb()?;
        match false_stmt {
          None => {
            let br = context.dfg().new_value().branch(cond, true_bb, end_bb);
            context.switch_bb(br, Some(true_bb))?;
            true_stmt.generate(context)?;
          }
          Some(false_stmt) => {
            let false_bb = context.add_bb()?;

            let br = context.dfg().new_value().branch(cond, true_bb, false_bb);
            context.switch_bb(br, Some(true_bb))?;
            true_stmt.generate(context)?;
            let jump = context.dfg().new_value().jump(end_bb);
            context.switch_bb(jump, Some(false_bb))?;
            false_stmt.generate(context)?;
          }
        }
        let jump = context.dfg().new_value().jump(end_bb);
        context.switch_bb(jump, Some(end_bb))?;
      }
      Stmt::While(exp, stmt) => {
        let entry_bb = context.add_bb()?;
        let body_bb = context.add_bb()?;
        let end_bb = context.add_bb()?;

        let jump_into_entry = context.dfg().new_value().jump(entry_bb);
        context.switch_bb(jump_into_entry, Some(entry_bb))?;

        let cond = expr::generate(exp.as_ref(), context)?;
        let br = context.dfg().new_value().branch(cond, body_bb, end_bb);
        context.switch_bb(br, Some(body_bb))?;

        context.loop_jump_pt.push((end_bb, entry_bb));
        stmt.generate(context)?;
        context.loop_jump_pt.pop();
        let jump = context.dfg().new_value().jump(entry_bb);
        context.switch_bb(jump, Some(end_bb))?;
      }
      Stmt::Break => {
        if context.loop_jump_pt.len() == 0 {
          Err(CompileError("Cannot break outside of loop".into()))?;
        }
        let (end_bb, _) = context.loop_jump_pt.last().unwrap().clone();
        let jump = context.dfg().new_value().jump(end_bb);
        context.switch_bb(jump, None)?;
      }
      Stmt::Continue => {
        if context.loop_jump_pt.len() == 0 {
          Err(CompileError("Cannot continue outside of loop".into()))?;
        }
        let (_, entry_bb) = context.loop_jump_pt.last().unwrap().clone();
        let jump = context.dfg().new_value().jump(entry_bb);
        context.switch_bb(jump, None)?;
      }
      Stmt::Return(exp) => {
        let ret_val = expr::generate(exp.as_ref(), context)?;
        let ret = context.dfg().new_value().ret(Some(ret_val));
        context.switch_bb(ret, None)?;
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
  fn generate(&self, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
    for i in self.iter() {
      let name = i.ident.clone();
      let val = i.init_val.eval(context).ok_or(CompileError(format!(
        "Constexpr variable {} must be initialized with constant expression",
        &name
      )))?;
      if !context.symbol.insert(&name, Symbol::Const(val)) {
        return Err(CompileError(format!("Redefinition of variable {}", &name)))?;
      }
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
      if !context.symbol.insert(&name, Symbol::Var(alloc)) {
        return Err(CompileError(format!("Redefinition of variable {}", &name)))?;
      }
    }
    Ok(())
  }
}
