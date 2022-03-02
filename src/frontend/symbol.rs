use std::{collections::HashMap, sync::RwLock};

use koopa::ir::{Function, Value};
use once_cell::sync::Lazy;

use super::{consteval::ConstValue, ty};

#[derive(Debug, Clone)]
pub enum Symbol {
  Const(ConstValue),
  Var(ty::Tys, Value),
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
