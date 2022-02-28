mod error;
mod from_func;
mod from_value;
mod riscv;

use std::collections::HashMap;
use std::error::Error;
use std::sync::RwLock;

use koopa::ir::{Function, Program};
use once_cell::sync::Lazy;

static FUNC_NAMES: Lazy<RwLock<HashMap<Function, String>>> = Lazy::new(|| RwLock::default());

pub fn generate_riscv(ir: &Program) -> Result<Vec<String>, Box<dyn Error>> {
  let mut result = vec![];

  for &func in ir.func_layout() {
    let func_data = ir.func(func);
    let func_name = &func_data.name()[1..];
    result.push(format!("  .text"));
    result.push(format!("  .globl {}", func_name));
    FUNC_NAMES.write()?.insert(func, func_name.into());

    let asm = from_func::generate(func_name, &func_data)?;
    result.extend(asm.into_iter());
    result.push("".into());
  }

  Ok(result)
}
