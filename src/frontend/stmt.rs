use std::fmt::Debug;
use std::rc::Rc;

use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};
use koopa::ir::Value;

use super::ast::{BlockItem, Decl, Initializer, InitializerLike, Stmt, TypeSpec};
use super::consteval::{ConstValue, EvalError};
use super::error::CompileError;
use super::expr;
use super::decl::GenerateContext;
use super::symbol::Symbol;
use super::ty::{self, TyUtils};
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
      Stmt::Assign(lval, exp) => {
        let lval = expr::generate(lval, context)?;
        let exp = expr::generate(exp.as_ref(), context)?;
        // println!("ASSIGN-L: {:?}", context.dfg().value(lval));
        // println!("ASSIGN-R: {:?}", context.dfg().value(exp));
        let store = context.dfg().new_value().store(exp, lval);
        context.add_inst(store)?;
      }
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
        context.new_bb_set();
        let true_bb = context.add_bb("if_true")?;
        let end_bb = context.add_bb("if_end")?;
        match false_stmt {
          None => {
            let br = context.dfg().new_value().branch(cond, true_bb, end_bb);
            context.switch_bb(br, Some(true_bb))?;
            true_stmt.generate(context)?;
          }
          Some(false_stmt) => {
            let false_bb = context.add_bb("if_false")?;

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
        context.new_bb_set();
        let entry_bb = context.add_bb("while_entry")?;
        let body_bb = context.add_bb("while_body")?;
        let end_bb = context.add_bb("while_end")?;

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
        let ret_val = match exp {
          Some(e) => Some(expr::generate(e.as_ref(), context)?),
          None => None,
        };
        let ret = context.dfg().new_value().ret(ret_val);
        context.switch_bb(ret, None)?;
      }
    }
    Ok(())
  }
}

impl GenerateStmt for Decl {
  fn generate(&self, context: &mut GenerateContext) -> Result<()> {
    match self {
      Decl::Var(declaration) => {
        if declaration.ty == TypeSpec::Void {
          Err(CompileError::IllegalVoid)?;
        }
        for (decl, init) in &declaration.list {
          let (tys, name) = ty::parse(decl.as_ref(), Some(context))?;
          if declaration.is_const {
            // 局部常量声明
            let init = init
              .as_ref()
              .ok_or(CompileError::InitializerRequired(name.into()))?;
            let const_value = match init.eval(Some(context)) {
              Err(e) => Err({
                match e {
                  EvalError::NotConstexpr => CompileError::ConstexprRequired("常量初始化器"),
                  EvalError::CompileError(e) => e,
                }
              })?,
              Ok(exp) => match &exp {
                InitializerLike::Simple(exp) => ConstValue::int(*exp),
                InitializerLike::Aggregate(_) => {
                  let size = tys.get_array_size();
                  let layout = get_layout(&size, &exp, &mut || 0)?;
                  ConstValue::from(size, layout)
                }
              },
            };
            if !context.symbol.insert(name, Symbol::Const(const_value)) {
              Err(CompileError::Redefinition(name.into()))?;
            }
          } else {
            // 局部变量声明
            let alloc = context.dfg().new_value().alloc(tys.to_ir());
            context.add_inst(alloc)?;
            if let Some(ref init) = init {
              let init_value = init.to_value(context)?;
              match init_value {
                InitializerLike::Simple(exp) => {
                  let store = context.dfg().new_value().store(exp, alloc);
                  context.add_inst(store)?;
                }
                InitializerLike::Aggregate(_) => {
                  let size = tys.get_array_size();
                  let layout = get_layout(&size, &init_value, &mut || {
                    context.dfg().new_value().integer(0)
                  })?;
                  store_value_layout(size, alloc, layout, context)?;
                }
              }
            }
            context
              .dfg()
              .set_value_name(alloc, Some(format!("@{}", name)));
            if !context.symbol.insert(&name, Symbol::Var(tys, alloc)) {
              return Err(CompileError::Redefinition(name.into()))?;
            }
          }
        }
        Ok(())
      }
      Decl::Func(f) => Err(CompileError::Other(format!(
        "不能在块作用域内声明函数 {}",
        f.ident
      )))?,
    }
  }
}

/// 将聚合初始化器打平
pub fn get_layout<T: Clone + Copy + Debug, DefaultFn: FnMut() -> T>(
  size: &Vec<usize>,
  init: &InitializerLike<T>,
  default: &mut DefaultFn,
) -> Result<Vec<T>> {
  match init {
    InitializerLike::Simple(i) => Ok(vec![i.clone()]),
    InitializerLike::Aggregate(aggr) => {
      if size.len() == 0 {
        return match aggr.len() {
          0 => Ok(vec![default()]),
          1 => get_layout(size, aggr[0].as_ref(), default),
          _ => Err(CompileError::TooManyInitializers)?,
        };
      }
      let total = size.iter().fold(1, |acc, x| acc * x);
      if aggr.len() > total {
        Err(CompileError::TooManyInitializers)?;
      }

      let mut current = vec![];
      let new_size: Vec<_> = size[1..].iter().cloned().collect();
      let new_total = new_size.iter().fold(1, |acc, x| acc * x);

      let mut i_aggr = 0;
      let mut i_result = 0;
      while i_aggr < aggr.len() {
        let next_aggr;
        if matches!(aggr[i_aggr].as_ref(), InitializerLike::Simple(_)) {
          let mut temp_aggr = vec![];
          let mut j = 0;
          while j < new_total {
            temp_aggr.push(if i_aggr + j < aggr.len() {
              aggr[i_aggr + j].clone()
            } else {
              Rc::from(InitializerLike::Simple(default()))
            });
            j += 1;
          }
          next_aggr = Rc::from(InitializerLike::<T>::Aggregate(temp_aggr));
          i_aggr += j;
        } else {
          next_aggr = aggr[i_aggr].clone();
          i_aggr += 1;
        }
        let result = get_layout(&new_size, next_aggr.as_ref(), default)?;
        current.extend(result);
        i_result += new_total;
      }
      while i_result < total {
        current.push(default());
        i_result += 1;
      }
      // println!("size: {:?}, current: {:?}", size, current);
      Ok(current)
    }
  }
}

impl Initializer {
  fn to_value(&self, context: &mut GenerateContext) -> Result<InitializerLike<Value>> {
    match self {
      Initializer::Simple(exp) => Ok(InitializerLike::Simple(expr::generate(
        exp.as_ref(),
        context,
      )?)),
      Initializer::Aggregate(aggr) => {
        let mut result: Vec<Rc<_>> = vec![];
        for exp in aggr {
          result.push(exp.to_value(context)?.into());
        }
        Ok(InitializerLike::Aggregate(result))
      }
    }
  }
}

/// 将 IR Value 构成的扁平数组初始化值保存到内存
pub fn store_value_layout(
  size: Vec<usize>,
  base: Value,
  data: Vec<Value>,
  context: &mut GenerateContext,
) -> Result<()> {
  if size.len() == 0 {
    let store = context.dfg().new_value().store(data[0], base);
    context.add_inst(store)?;
  } else {
    let step = size[1..].iter().fold(1, |acc, x| acc * x);
    for i in 0..size[0] {
      let start_index = i * step;
      let end_index = start_index + step;
      let index = context.dfg().new_value().integer(i as i32);
      let ptr = context.dfg().new_value().get_elem_ptr(base, index);
      context.add_inst(ptr)?;
      store_value_layout(
        size[1..].into(),
        ptr,
        data[start_index..end_index].into(),
        context,
      )?;
    }
  }
  Ok(())
}
