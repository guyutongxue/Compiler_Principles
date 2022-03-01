use koopa::ir::builder::LocalInstBuilder;
use koopa::ir::Type;

use super::ast::{BlockItem, Decl, Initializer, LVal, Stmt, TypeSpec};
use super::consteval::{consteval, EvalError};
use super::error::CompileError;
use super::ir::GenerateContext;
use super::symbol::{Symbol, SymbolTable};
use super::{expr, ty};
use crate::Result;

#[allow(unused_imports)]
use super::error::UnimplementedError;

pub fn generate(item: &BlockItem, context: &mut GenerateContext) -> Result<()> {
  match item {
    BlockItem::Stmt(stmt) => stmt.generate(context),
    BlockItem::Decl(decl) => decl.generate(context),
  }
}

trait GenerateStmt {
  fn generate(&self, context: &mut GenerateContext) -> Result<()>;
}

impl GenerateStmt for Stmt {
  fn generate(&self, context: &mut GenerateContext) -> Result<()> {
    match self {
      Stmt::Assign(lval, exp) => match lval {
        LVal::Ident(ident) => {
          let symbol = context
            .symbol
            .get(ident)
            .or_else(|| SymbolTable::get_global(ident))
            .ok_or(CompileError::UndeclaredSymbol(ident.clone()))?;
          match symbol {
            Symbol::Const(_) => Err(CompileError::CanNotAssign(ident.clone(), "常量"))?,
            Symbol::Var(alloc) => {
              let exp = expr::generate(exp.as_ref(), context)?;
              let store = context.dfg().new_value().store(exp, alloc);
              context.add_inst(store)?;
            }
            Symbol::Func(_) => Err(CompileError::CanNotAssign(ident.clone(), "函数"))?,
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
          Err(CompileError::IllegalBreak)?;
        }
        let (end_bb, _) = context.loop_jump_pt.last().unwrap().clone();
        let jump = context.dfg().new_value().jump(end_bb);
        context.switch_bb(jump, None)?;
      }
      Stmt::Continue => {
        if context.loop_jump_pt.len() == 0 {
          Err(CompileError::IllegalContinue)?;
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
  fn generate(&self, context: &mut GenerateContext) -> Result<()> {
    match self {
      // Decl::Const(ty, decl) => {
      //   if *ty == TypeSpec::Void {
      //     Err(CompileError("Cannot declare variable of type void".into()))?;
      //   }
      //   for i in decl {
      //     let name = i.name.clone();
      //     let val = consteval(i.init_val.as_ref(), context).ok_or(CompileError(format!(
      //       "Constexpr variable {} must be initialized with constant expression",
      //       &name
      //     )))?;
      //     if !context.symbol.insert(&name, Symbol::Const(val)) {
      //       return Err(CompileError(format!("Redefinition of variable {}", &name)))?;
      //     }
      //   }
      //   Ok(())
      // }
      Decl::Var(declaration) => {
        if declaration.ty == TypeSpec::Void {
          Err(CompileError::IllegalVoid)?;
        }
        for (decl, init) in &declaration.list {
          let (ty, name) = ty::parse(decl.as_ref())?;
          if declaration.is_const {
            let init = init
              .as_ref()
              .ok_or(CompileError::InitializerRequired(name.into()))?;
            let value = match init {
              Initializer::Simple(init) => {
                consteval(init.as_ref(), context).map_err(|e| match e {
                  EvalError::NotConstexpr => CompileError::ConstexprRequired("常量初始化器"),
                  EvalError::CompileError(e) => e,
                })?
              }
              Initializer::Aggregate(_) => {
                Err("Not impl: local const decl aggregate initializer".to_string())?
              }
            };
            if !context.symbol.insert(&name, Symbol::Const(value)) {
              return Err(CompileError::Redefinition(name.into()))?;
            }
          } else {
            let alloc = context.dfg().new_value().alloc(Type::get_i32());
            context.add_inst(alloc)?;
            if let Some(ref init) = init {
              match init {
                Initializer::Simple(exp) => {
                  let init_val = expr::generate(exp.as_ref(), context)?;
                  let store = context.dfg().new_value().store(init_val, alloc);
                  context.add_inst(store)?;
                }
                Initializer::Aggregate(_) => {
                  Err("Not impl: local decl aggregate initializer".to_string())?
                }
              }
            }
            if !context.symbol.insert(&name, Symbol::Var(alloc)) {
              return Err(CompileError::Redefinition(name.into()))?;
            }
          }
        }
        Ok(())
      }
      Decl::Func(_) => Err(CompileError::Other(
        "Cannot declare function in block".into(),
      ))?,
    }
  }
}
