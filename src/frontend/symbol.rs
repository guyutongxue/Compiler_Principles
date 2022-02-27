use std::collections::HashMap;

use koopa::ir::Value;

#[derive(Debug, Clone, Copy)]
pub enum Symbol {
  Const(i32),
  Var(Value),
}

pub struct SymbolTable(Vec<HashMap<String, Symbol>>);

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

  pub fn get(&self, key: &str) -> Option<Symbol> {
    for i in self.0.iter().rev() {
      if let Some(v) = i.get(key) {
        return Some(v.clone());
      }
    }
    None
  }

  pub fn push(&mut self) {
    self.0.push(HashMap::new());
  }

  pub fn pop(&mut self) -> bool {
    self.0.pop().is_some()
  }
}
