use std::{collections::HashMap, sync::RwLock};

use koopa::ir::{Function, Type, Value};
use once_cell::sync::Lazy;

use super::error::CompileError;
use super::expr::ty::SysyType;

#[derive(Debug, Clone)]
pub struct ConstValue {
  pub data: Vec<i32>,

  /// 当以变量下标访问常量数组时，仍然需要生成数组的 IR
  pub value: Option<Value>,

  /// [] 表示变量，即零维数组
  pub size: Vec<usize>,
}

impl ConstValue {
  pub fn int(number: i32) -> Self {
    Self {
      data: vec![number],
      value: None,
      size: vec![],
    }
  }

  pub fn from(size: Vec<usize>, data: Vec<i32>) -> Self {
    Self {
      data,
      value: None,
      size,
    }
  }

  pub fn as_int(&self) -> std::result::Result<i32, CompileError> {
    if self.size.len() == 0 {
      Ok(self.data[0])
    } else {
      Err(CompileError::TypeMismatch("整数", "".into(), "数组"))?
    }
  }

  pub fn item(&self, index: i32) -> std::result::Result<Self, CompileError> {
    if self.size.len() == 0 {
      Err(CompileError::TypeMismatch("数组", "".into(), "变量"))?
    }
    if index < 0 || index as usize >= self.size[0] {
      Err(CompileError::IndexOutOfBounds(index, self.size[0]))?
    }
    let index = index as usize;
    let step = self.size[1..].iter().fold(1, |acc, &x| acc * x);
    let start_index = index * step;
    let end_index = start_index + step;
    Ok(Self::from(
      self.size[1..].into(),
      self.data[start_index..end_index].into(),
    ))
  }

  pub fn ir_type(&self) -> Type {
    fn ir_type_impl(size: &[usize]) -> Type {
      if size.len() == 0 {
        Type::get_i32()
      } else {
        Type::get_array(ir_type_impl(size[1..].as_ref()), size[0])
      }
    }
    ir_type_impl(self.size.as_ref())
  }
}

#[derive(Debug, Clone)]
pub enum Symbol {
  Const(ConstValue),
  Var(SysyType, Value),
  Func(Function),
}

pub struct SymbolTable(Vec<HashMap<String, Symbol>>);

/// (符号名, 存在定义)
static GLOBAL: Lazy<RwLock<HashMap<String, (Symbol, bool)>>> = Lazy::new(|| RwLock::default());

impl SymbolTable {
  pub fn new() -> SymbolTable {
    SymbolTable(vec![HashMap::new()])
  }

  #[must_use]
  pub fn insert(&mut self, key: &str, value: Symbol) -> bool {
    if self.0.len() == 0 {
      return false;
    }
    let current = self.0.last_mut().unwrap();
    current.insert(key.into(), value).is_none()
  }

  #[must_use]
  pub fn insert_global_def(key: &str, value: Symbol) -> bool {
    let exists = GLOBAL.write().unwrap().insert(key.into(), (value, true));
    if let Some(exists) = exists {
      if exists.1 {
        return false;
      }
    }
    true
  }

  pub fn insert_global_decl(key: &str, value: Symbol) -> bool {
    GLOBAL
      .write()
      .unwrap()
      .insert(key.into(), (value, false))
      .is_none()
  }

  pub fn get(&self, key: &str) -> Option<Symbol> {
    for i in self.0.iter().rev() {
      if let Some(v) = i.get(key) {
        return Some(v.clone());
      }
    }
    None
  }

  pub fn get_global(key: &str) -> Option<Symbol> {
    GLOBAL.read().ok()?.get(key).cloned().map(|v| v.0)
  }

  pub fn push(&mut self) {
    self.0.push(HashMap::new());
  }

  pub fn pop(&mut self) -> bool {
    self.0.pop().is_some()
  }
}
