use koopa::ir::{Type, TypeKind};

use super::consteval::Eval;
use crate::frontend::ast::{
  AddExp, Declarator, EqExp, FuncDecl, LAndExp, LOrExp, MulExp, PrimaryExp, RelExp, TypeSpec,
  UnaryExp, Exp, AssignExp, PostfixExp,
};
use crate::frontend::decl::GenerateContext;
use crate::frontend::error::CompileError;
use crate::frontend::symbol::{Symbol, SymbolTable};
use crate::Result;

#[derive(Debug, Clone, PartialEq)]
pub enum SysyType {
  Int,
  Void,
  Array(Box<SysyType>, usize),
  Pointer(Box<SysyType>),
  Function(Box<SysyType>, Vec<Box<SysyType>>),
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
          let value = exp
            .eval(context)
            .map_err(|e| e.to_compile_error("数组长度"))?;
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
      SysyType::Void => Type::get_unit(),
      SysyType::Array(ele, len) => Type::get_array(ele.to_ir(), *len),
      SysyType::Pointer(base) => Type::get_pointer(base.to_ir()),
      SysyType::Function(ret, params) => {
        Type::get_function(params.iter().map(|p| p.to_ir()).collect(), ret.to_ir())
      }
    }
  }

  pub fn from_ir(ty: &Type) -> SysyType {
    match ty.kind() {
      TypeKind::Int32 => SysyType::Int,
      TypeKind::Unit => SysyType::Void,
      TypeKind::Array(base, len) => SysyType::Array(Box::new(SysyType::from_ir(base)), *len),
      TypeKind::Pointer(base) => SysyType::Pointer(Box::new(SysyType::from_ir(base))),
      TypeKind::Function(params, ret) => SysyType::Function(
        Box::new(SysyType::from_ir(ret)),
        params.iter().map(|p| SysyType::from_ir(p).into()).collect(),
      ),
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
      SysyType::Void => vec![],
      SysyType::Pointer(_) => vec![],
      SysyType::Function(..) => vec![],
      SysyType::Array(ele, len) => {
        let mut size = vec![*len];
        size.extend(ele.get_array_size());
        size
      }
    }
  }

  pub fn is_int(&self) -> bool {
    matches!(self, SysyType::Int)
  }

  pub fn decay(&self) -> Self {
    match self {
      SysyType::Array(ele, _) => SysyType::Pointer(ele.clone()),
      SysyType::Function(..) => SysyType::Pointer(self.clone().into()),
      _ => self.clone(),
    }
  }
}

pub trait GetType {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType>;
}

impl GetType for Exp {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    match self {
      Exp::Assign(exp) => exp.get_type(context),
      Exp::Comma(_, rhs) => rhs.get_type(context),
    }
  }
}

impl GetType for AssignExp {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    match self {
      AssignExp::LOr(exp) => exp.get_type(context),
      AssignExp::Assign(lhs, rhs) => {
        let lhs_ty = lhs.get_type(context)?;
        let rhs_ty = rhs.get_type(context)?;
        if matches!(lhs_ty, SysyType::Array(..)) {
          return Err(CompileError::TypeMismatch("可修改左值", "".into(), "数组"))?;
        } 
        if lhs_ty != rhs_ty {
          return Err(CompileError::TypeMismatch("左值", "".into(), "不匹配的类型"))?;
        }
        Ok(lhs_ty)
      }
    }
  }
}

impl GetType for LOrExp {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    match self {
      LOrExp::And(exp) => exp.get_type(context),
      LOrExp::Or(lhs, rhs) => {
        let lhs_ty = lhs.get_type(context)?;
        let rhs_ty = rhs.get_type(context)?;
        if !lhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else if !rhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else {
          Ok(SysyType::Int)
        }
      }
    }
  }
}

impl GetType for LAndExp {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    match self {
      LAndExp::Eq(exp) => exp.get_type(context),
      LAndExp::And(lhs, rhs) => {
        let lhs_ty = lhs.get_type(context)?;
        let rhs_ty = rhs.get_type(context)?;
        if !lhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else if !rhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else {
          Ok(SysyType::Int)
        }
      }
    }
  }
}

impl GetType for EqExp {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    match self {
      EqExp::Rel(exp) => exp.get_type(context),
      EqExp::Eq(lhs, _, rhs) => {
        let lhs_ty = lhs.get_type(context)?;
        let rhs_ty = rhs.get_type(context)?;
        if !lhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else if !rhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else {
          Ok(SysyType::Int)
        }
      }
    }
  }
}

impl GetType for RelExp {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    match self {
      RelExp::Add(exp) => exp.get_type(context),
      RelExp::Rel(lhs, _, rhs) => {
        let lhs_ty = lhs.get_type(context)?;
        let rhs_ty = rhs.get_type(context)?;
        if !lhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else if !rhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else {
          Ok(SysyType::Int)
        }
      }
    }
  }
}

impl GetType for AddExp {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    match self {
      AddExp::Mul(exp) => exp.get_type(context),
      AddExp::Add(lhs, _, rhs) => {
        let lhs_ty = lhs.get_type(context)?;
        let rhs_ty = rhs.get_type(context)?;
        if !lhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else if !rhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else {
          Ok(SysyType::Int)
        }
      }
    }
  }
}

impl GetType for MulExp {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    match self {
      MulExp::Unary(exp) => exp.get_type(context),
      MulExp::Mul(lhs, _, rhs) => {
        let lhs_ty = lhs.get_type(context)?;
        let rhs_ty = rhs.get_type(context)?;
        if !lhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else if !rhs_ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else {
          Ok(SysyType::Int)
        }
      }
    }
  }
}

impl GetType for UnaryExp {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    match self {
      UnaryExp::Postfix(exp) => exp.get_type(context),
      UnaryExp::Address(exp) => Ok(SysyType::Pointer(exp.get_type(context)?.into())),
      UnaryExp::Deref(exp) => {
        let ty = exp.get_type(context)?;
        match ty {
          SysyType::Pointer(ty) => Ok(ty.as_ref().clone()),
          _ => Err(CompileError::TypeMismatch("指针", "".into(), "?"))?,
        }
      }
      UnaryExp::Op(_, exp) => {
        let ty = exp.get_type(context)?;
        if !ty.is_int() {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        } else {
          Ok(SysyType::Int)
        }
      }
    }
  }
}

impl GetType for PostfixExp {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    match self {
      PostfixExp::Primary(exp) => exp.get_type(context),
      PostfixExp::Call(lhs, args) => {
        let (ret, params) = match SymbolTable::get_global(&lhs) {
          Some(Symbol::Func(SysyType::Function(ret, args), _)) => (ret, args),
          _ => Err(CompileError::TypeMismatch("函数", "".into(), "?"))?,
        };
        if params.len() != args.len() {
          Err(CompileError::TypeMismatch(
            "函数",
            "".into(),
            "数量不同的参数",
          ))?
        }
        for (p, a) in Iterator::zip(params.iter(), args.iter()) {
          if p.as_ref().clone() != a.get_type(context)?.decay() {
            Err(CompileError::TypeMismatch("?", "".into(), "错误的参数类型"))?
          }
        }
        Ok(ret.as_ref().clone())
      }
      PostfixExp::Subscript(lval, exp) => {
        let exp_ty = exp.get_type(context)?;
        if exp_ty != SysyType::Int {
          Err(CompileError::TypeMismatch("整数", "".into(), "?"))?
        }
        match lval.get_type(context)? {
          SysyType::Array(ele, _) => Ok(ele.as_ref().clone()),
          SysyType::Pointer(ele) => Ok(ele.as_ref().clone()),
          _ => Err(CompileError::TypeMismatch("数组", "".into(), "?"))?,
        }
      }
    }
  }
}

impl GetType for FuncDecl {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    let params_ty = self
      .params
      .iter()
      .map(|arg| {
        let (ty, _) = SysyType::parse(arg, context)?;
        Ok(ty.into())
      })
      .collect::<Result<Vec<_>>>()?;
    let return_ty: Box<_> = match self.func_type {
      TypeSpec::Int => SysyType::Int.into(),
      TypeSpec::Void => SysyType::Void.into(),
    };
    Ok(SysyType::Function(return_ty, params_ty))
  }
}

impl GetType for PrimaryExp {
  fn get_type(&self, context: Option<&GenerateContext>) -> Result<SysyType> {
    match self {
      PrimaryExp::Ident(ident) => {
        let symbol = match context {
          Some(context) => context.symbol.get(ident).or_else(|| SymbolTable::get_global(ident)),
          None => SymbolTable::get_global(ident),
        };
        let ty = match symbol {
          Some(symbol) => match symbol {
            Symbol::Const(cv) => cv.ty,
            Symbol::Var(ty, _) => ty,
            Symbol::Func(ty, _) => ty,
          },
          None => Err(CompileError::UndeclaredSymbol(ident.clone()))?,
        };
        Ok(ty)
      }
      PrimaryExp::Num(_) => Ok(SysyType::Int),
      PrimaryExp::Paren(exp) => exp.get_type(context),
    }
  }
}
