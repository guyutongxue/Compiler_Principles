
use super::ast::Declarator;
use super::consteval::{Eval, EvalError};
use super::error::CompileError;
use crate::Result;

pub enum Ty {
  Int,
  Pointer(Box<Ty>),
  Array(Box<Ty>, u32),
}

pub fn parse(declarator: &Declarator) -> Result<(Ty, &str)> {
  match declarator {
    Declarator::Ident(ident) => Ok((Ty::Int, ident)),
    Declarator::Pointer(decl) => {
      let (ty, ident) = parse(decl)?;
      Ok((Ty::Pointer(Box::new(ty)), ident))
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
      let (ty, ident) = parse(decl)?;
      Ok((Ty::Array(Box::new(ty), len as u32), ident))
    }
  }
}
