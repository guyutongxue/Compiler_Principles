use std::{collections::HashMap, error::Error};

use super::from_value;
use super::riscv::{inst::Inst, reg::Reg};
use koopa::ir::{FunctionData, Value, BasicBlock};

pub struct GenerateContext<'a> {
  /// 变量到内存位置（距离栈指针偏差）的映射
  pub offsets: HashMap<Value, i32>,
  /// 下一个映射位置（分配局部变量时使用）
  pub next_offset: Box<dyn Iterator<Item = i32>>,
  /// 栈帧大小
  pub frame_size: i32,

  /// IR 基本块到汇编标签的映射
  pub labels: HashMap<BasicBlock, String>,

  /// IR 数据
  pub func_data: &'a FunctionData,
  /// 已生成指令序列
  pub insts: Vec<String>,
}

impl<'a> GenerateContext<'a> {
  fn from(func: &'a FunctionData) -> Self {
    let inst_num: i32 = func
      .layout()
      .bbs()
      .iter()
      .map(|(_, node)| {
        node
          .insts()
          .iter()
          .filter(|(&v, _)| !func.dfg().value(v).ty().is_unit())
          .count() as i32
      })
      .sum();
    let size = (inst_num * 4 + 15) & !15;

    // PROLOGUE
    let prologue = Inst::Addi(Reg::Sp, Reg::Sp, -size).to_string();

    Self {
      offsets: HashMap::new(),
      next_offset: Box::new((0..).map(|i| i * 4)),
      frame_size: size,

      labels: HashMap::new(),

      func_data: func,
      insts: vec![prologue],
    }
  }
}

pub fn generate(func_name: &str, func_data: &FunctionData) -> Result<Vec<String>, Box<dyn Error>> {
  let mut asm: Vec<String> = vec![format!("{}:", func_name)];

  let mut context = GenerateContext::from(&func_data);

  // Generate map from BB to label
  for (&bb, _) in func_data.layout().bbs() {
    let bb_name = &func_data.dfg().bb(bb).name().clone().unwrap()[1..];
    let label_name = format!("{}_{}", func_name, bb_name);
    context.labels.insert(bb, label_name.clone());
  }

  for (&bb, node) in func_data.layout().bbs() {
    let label = context.get_label(bb)?;
    asm.push(format!("{}:", label));
    for &i in node.insts().keys() {
      from_value::generate(i, &mut context)?;
    }
    asm.append(&mut context.insts);
  }
  Ok(asm)
}
