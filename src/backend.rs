mod error;
mod from_func;
mod from_value;
mod riscv;

use std::collections::HashMap;
use std::sync::RwLock;

use koopa::ir::{Function, Program, Value, ValueKind};
use once_cell::sync::Lazy;

use self::error::LabelNotExistError;
use crate::Result;

static FUNC_NAMES: Lazy<RwLock<HashMap<Function, String>>> = Lazy::new(|| RwLock::default());
static VAR_NAMES: Lazy<RwLock<HashMap<Value, String>>> = Lazy::new(|| RwLock::default());

pub fn generate_riscv(ir: &Program) -> Result<Vec<String>> {
  let mut result = vec![];

  for (&v, vd) in ir.borrow_values().iter() {
    if let ValueKind::GlobalAlloc(alloc) = vd.kind() {
      let name = vd
        .name()
        .clone()
        .ok_or(LabelNotExistError("alloc ???".into()))?;
      let name = name[1..].to_string();
      result.push("  .data".into());
      result.push(format!("  .globl {}", &name));
      result.push(format!("{}:", &name));
      VAR_NAMES.write()?.insert(v, name);
      match ir.borrow_value(alloc.init()).kind() {
        ValueKind::Integer(i) => {
          let i = i.value();
          result.push(format!("  .word {}", i));
        }
        ValueKind::ZeroInit(_) => {
          result.push("  .zero 4".into());
        }
        _ => {}
      }
      result.push("".into());
    }
  }

  for &func in ir.func_layout() {
    let func_data = ir.func(func);
    let func_name = &func_data.name()[1..];
    FUNC_NAMES.write()?.insert(func, func_name.into());

    if func_data.layout().entry_bb().is_none() {
      // Function declaration, skip.
      continue;
    }

    result.push("  .text".into());
    result.push(format!("  .globl {}", func_name));

    let asm = from_func::generate(func_name, &func_data)?;
    result.extend(asm.into_iter());
    result.push("".into());
  }
  Ok(result)
}
