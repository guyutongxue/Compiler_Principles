mod error;
mod riscv;
mod from_value;
mod from_func;

use koopa::ir::Program;
use std::error::Error;

pub fn generate_riscv(ir: &Program) -> Result<Vec<String>, Box<dyn Error>> {
  let mut decls: Vec<String> = vec!["  .text".into()];
  let mut text_asms: Vec<String> = vec![];

  for &func in ir.func_layout() {
    let func_data = ir.func(func);
    let func_name = &func_data.name()[1..];
    decls.push(format!("  .globl {}", func_name));

    let asm = from_func::generate(func_name, &func_data)?;
    text_asms.extend(asm.into_iter());
  }
  decls.extend(text_asms.into_iter());
  decls.push("".into());

  Ok(decls)
}
