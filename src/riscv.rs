use std::error::Error;

use koopa::ir::{FunctionData, Program, Value, ValueKind};

pub trait GenerateAsm {
  fn generate(&self) -> Result<String, Box<dyn Error>>;
}

impl GenerateAsm for Program {
  fn generate(&self) -> Result<String, Box<dyn Error>> {
    let mut decls: Vec<String> = vec!["  .text".into()];
    let mut text_asms: Vec<String> = vec![];

    for &func in self.func_layout() {
      let func_data = self.func(func);
      let func_name = &func_data.name()[1..];
      decls.push(format!("  .globl {}", func_name));
      let mut fn_asms: Vec<String> = vec![format!("{}:", func_name)];

      for (&bb, node) in func_data.layout().bbs() {
        let bb_name = &func_data.dfg().bb(bb).name().clone().unwrap()[1..];
        let mut insts: Vec<String> = vec![format!("{}:", bb_name)];
        for &inst in node.insts().keys() {
          insts.push(inst.generate(&func_data)?);
        }
        fn_asms.push(insts.join("\n"));
      }

      text_asms.push(fn_asms.join("\n") + "\n");
    }
    Ok(decls.join("\n") + "\n" + &text_asms.join("\n"))
  }
}

trait GenerateAsmDetail {
  fn generate(self, func_data: &FunctionData) -> Result<String, Box<dyn Error>>;
}

impl GenerateAsmDetail for Value {
  fn generate(self, func_data: &FunctionData) -> Result<String, Box<dyn Error>> {
    let data = func_data.dfg().value(self);
    match data.kind() {
      ValueKind::Integer(num) => Ok(format!("{}", num.value())),

      ValueKind::Return(ret) => {
        let retval = ret.value().unwrap().generate(&func_data)?;
        Ok(format!("  li a0, {}\n  ret", &retval))
      }
      _ => panic!("unimplemented"),
    }
  }
}
