use std::{collections::HashMap, sync::Mutex};

use once_cell::sync::Lazy;

#[derive(Debug)]
pub enum Symbol {
  ConstSymbol(i32),
}

pub static SYMBOLS: Lazy<Mutex<HashMap<String, Symbol>>> = Lazy::new(|| Mutex::default());
