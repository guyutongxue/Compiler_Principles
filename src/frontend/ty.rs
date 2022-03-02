
use koopa::ir::Type;

use super::ast::Declarator;
use super::consteval::{Eval, EvalError};
use super::error::CompileError;
use super::ir::GenerateContext;
use crate::Result;

pub type Tys = Vec<Ty>;

#[derive(Debug, Clone)]
pub enum Ty {
  Pointer,
  Array(usize),
}

pub fn parse<'a>(declarator: &'a Declarator, context: Option<&GenerateContext>) -> Result<(Vec<Ty>, &'a str)> {
  match declarator {
    Declarator::Ident(ident) => Ok((vec![], ident)),
    Declarator::Pointer(decl) => {
      let (mut tys, ident) = parse(decl, context)?;
      tys.push(Ty::Pointer);
      Ok((tys, ident))
    }
    Declarator::Array(decl, exp) => {
      let value = exp.eval(context).map_err(|e| match e {
        EvalError::NotConstexpr => CompileError::ConstexprRequired("数组长度"),
        EvalError::CompileError(e) => e,
      })?;
      let len = value.as_int()?;
      if len <= 0 {
        return Err(CompileError::NegativeSubscript(len))?;
      }
      let (mut tys, ident) = parse(decl, context)?;
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
    if self.len() == 0 {
      Type::get_i32()
    } else {
      match self[0] {
        Ty::Array(len) => Type::get_array(self[1..].to_vec().to_ir(), len),
        Ty::Pointer => Type::get_pointer(self[1..].to_vec().to_ir()),
      }
    }
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
