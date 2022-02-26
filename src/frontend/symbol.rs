use std::{collections::HashMap, sync::{RwLock}};

use koopa::ir::Value;
use once_cell::sync::Lazy;

#[derive(Debug)]
pub enum Symbol {
  Const(i32),
  Var(Value),
}

pub static SYMBOLS: Lazy<RwLock<HashMap<String, Symbol>>> = Lazy::new(|| RwLock::default());
