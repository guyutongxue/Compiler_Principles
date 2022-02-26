use std::{collections::HashMap, error::Error};

use koopa::ir::{FunctionData, Value};
use super::{from_value, riscv::reg::Reg};

pub struct GenerateContext<'a> {
  pub regs: HashMap<Value, Reg>,
  pub next_reg: core::slice::Iter<'static, Reg>,
  pub func_data: &'a FunctionData,
  pub insts: Vec<String>,
}

impl<'a> GenerateContext<'a> {
  fn from(func: &'a FunctionData) -> Self {
    Self {
      regs: HashMap::new(),
      next_reg: from_value::TEMP_REGS.iter(),
      func_data: func,
      insts: vec![],
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
