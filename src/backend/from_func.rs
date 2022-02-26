use std::{collections::HashMap, error::Error, iter};

use super::from_value;
use super::riscv::{inst::Inst, reg::Reg};
use koopa::ir::{FunctionData, Value};

pub struct GenerateContext<'a> {
  pub offsets: HashMap<Value, i32>,
  pub next_offset: Box<dyn Iterator<Item = i32>>,
  pub frame_size: i32,
  pub func_data: &'a FunctionData,
  pub insts: Vec<String>,
}

impl<'a> GenerateContext<'a> {
  fn from(func: &'a FunctionData) -> Self {
    let inst_num = func
      .dfg()
      .values()
      .values()
      .filter(|vd| !vd.ty().is_unit())
      .count() as i32;
    let size = (inst_num * 4 + 15) & !15;
    let mut cnt = size;
    let counter = iter::from_fn(move || {
      cnt -= 4;
      if cnt < 0 {
        None
      } else {
        Some(cnt)
      }
    });

    // PROLOGUE
    let prologue = Inst::Addi(Reg::Sp, Reg::Sp, -size).to_string();

    Self {
      offsets: HashMap::new(),
      next_offset: Box::new(counter),
      frame_size: size,
      func_data: func,
      insts: vec![prologue],
    }
  }
}

pub fn generate(func_name: &str, func_data: &FunctionData) -> Result<Vec<String>, Box<dyn Error>> {
  let mut asm: Vec<String> = vec![format!("{}:", func_name)];

  let mut context = GenerateContext::from(&func_data);

  for (&bb, node) in func_data.layout().bbs() {
    let bb_name = &func_data.dfg().bb(bb).name().clone().unwrap()[1..];
    asm.push(format!("{}_{}:", func_name, bb_name));
    for i in node.insts().keys() {
      from_value::generate(*i, &mut context)?;
    }
  }
  asm.extend(context.insts.iter().map(|s| format!("  {}", s)));
  Ok(asm)
}
