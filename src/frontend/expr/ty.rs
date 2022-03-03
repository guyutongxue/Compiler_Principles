use koopa::ir::Type;

use super::consteval::{Eval};
use crate::frontend::ast::Declarator;
use crate::frontend::decl::GenerateContext;
use crate::frontend::error::CompileError;
use crate::Result;

#[derive(Debug, Clone)]
pub enum SysyType {
  Int,
  Array(Box<SysyType>, usize),
  Pointer(Box<SysyType>),
}

impl SysyType {

  /// 从 AST 获取声明符的类型和名字
  pub fn parse<'a>(
    declarator: &'a Declarator,
    context: Option<&GenerateContext>,
  ) -> Result<(SysyType, &'a str)> {
    enum TyImpl {
      Array(usize),
      Pointer,
    }

    fn decl_to_vec<'a>(
      declarator: &'a Declarator,
      context: Option<&GenerateContext>,
    ) -> Result<(Vec<TyImpl>, &'a str)> {
      match declarator {
        Declarator::Ident(ident) => Ok((vec![], ident)),
        Declarator::Pointer(decl) => {
          let (mut tys, ident) = decl_to_vec(decl, context)?;
          tys.push(TyImpl::Pointer);
          Ok((tys, ident))
        }
        Declarator::Array(decl, exp) => {
          let value = exp.eval(context).map_err(|e| e.to_compile_error("数组长度"))?;
          let len = value.as_int()?;
          if len <= 0 {
            return Err(CompileError::NegativeSubscript(len))?;
          }
          let (mut tys, ident) = decl_to_vec(decl, context)?;
          tys.push(TyImpl::Array(len as usize));
          Ok((tys, ident))
        }
      }
    }

    let (vec, ident) = decl_to_vec(declarator, context)?;

    fn vec_to_ty(vec: &[TyImpl]) -> SysyType {
      if vec.len() == 0 {
        return SysyType::Int;
      }
      match vec[0] {
        TyImpl::Pointer => SysyType::Pointer(Box::new(vec_to_ty(&vec[1..]))),
        TyImpl::Array(len) => SysyType::Array(Box::new(vec_to_ty(&vec[1..])), len),
      }
    }

    Ok((vec_to_ty(&vec), ident))
  }

  /// 将 SysY 类型转换为 Koopa IR 类型
  pub fn to_ir(&self) -> Type {
    match self {
      SysyType::Int => Type::get_i32(),
      SysyType::Array(ele, len) => Type::get_array(ele.to_ir(), *len),
      SysyType::Pointer(base) => Type::get_pointer(base.to_ir()),
    }
  }

  /// 获取数组大小。
  /// 
  /// 比如 `int a[2][3]` 返回 `[2, 3]`；
  /// 
  /// 若不是数组，返回 `[]`。
  pub fn get_array_size(&self) -> Vec<usize> {
    match self {
      SysyType::Int => vec![],
      SysyType::Array(ele, len) => {
        let mut size = vec![*len];
        size.extend(ele.get_array_size());
        size
      }
      SysyType::Pointer(_) => vec![],
    }
  }
}
