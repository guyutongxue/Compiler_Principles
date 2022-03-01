
use koopa::ir::Type;

use super::ast::Declarator;
use super::consteval::{Eval, EvalError};
use super::error::CompileError;
use crate::Result;

pub type Tys = Vec<Ty>;

#[derive(Debug)]
pub enum Ty {
  Pointer,
  Array(usize),
}

pub fn parse(declarator: &Declarator) -> Result<(Vec<Ty>, &str)> {
  match declarator {
    Declarator::Ident(ident) => Ok((vec![], ident)),
    Declarator::Pointer(decl) => {
      let (mut tys, ident) = parse(decl)?;
      tys.push(Ty::Pointer);
      Ok((tys, ident))
    }
    Declarator::Array(decl, exp) => {
      let value = exp.eval(None).map_err(|e| match e {
        EvalError::NotConstexpr => CompileError::ConstexprRequired("数组长度"),
        EvalError::CompileError(e) => e,
      })?;
      let len = value.as_int()?;
      if len <= 0 {
        return Err(CompileError::NegativeSubscript(len, "作为数组长度"))?;
      }
      let (mut tys, ident) = parse(decl)?;
      tys.push(Ty::Array(len as usize));
      Ok((tys, ident))
    }
  }
}

pub trait TyUtils {
  fn to_ir(&self) -> Type;
  fn get_array_size(&self) -> Vec<usize>;
}

impl TyUtils for Tys {
  fn to_ir(&self) -> Type {
    let mut ir_ty = Type::get_i32();
    for ty in self {
      match ty {
        Ty::Array(len) => ir_ty = Type::get_array(ir_ty, *len),
        Ty::Pointer => ir_ty = Type::get_pointer(ir_ty),
      }
    }
    ir_ty
  }

  fn get_array_size(&self) -> Vec<usize> {
    let mut size = vec![];
    for ty in self {
      match ty {
        Ty::Array(len) => size.push(*len),
        Ty::Pointer => return size,
      }
    }
    size
  }
}
