use std::cmp;
use std::collections::HashMap;

use koopa::ir::{BasicBlock, FunctionData, Value, ValueKind};

use super::error::LabelNotExistError;
use super::from_value;
use super::riscv::{inst::Inst, reg::Reg};
use crate::Result;

static CALL_REGS: [Reg; 8] = [
  Reg::A0,
  Reg::A1,
  Reg::A2,
  Reg::A3,
  Reg::A4,
  Reg::A5,
  Reg::A6,
  Reg::A7,
];

pub struct GenerateContext<'a> {
  /// 变量到内存位置（距离栈指针偏差）的映射
  pub offsets: HashMap<Value, i32>,
  /// 下一个映射位置（分配局部变量时使用）
  pub next_offset: Box<dyn Iterator<Item = i32>>,

  size_r: i32,
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
  fn from(func: &'a FunctionData) -> Result<Self> {
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

    let size_s = 4 * inst_num;

    let calls: Vec<_> = func
      .dfg()
      .values()
      .iter()
      .filter_map(|(_, vd)| match vd.kind() {
        ValueKind::Call(func) => Some(func.args().len() as i32),
        _ => None,
      })
      .collect();
    let size_r = if calls.len() > 0 { 4 } else { 0 };
    let size_a = calls
      .iter()
      .map(|len| cmp::max(len - 8, 0))
      .max()
      .unwrap_or(0)
      * 4;

    let size = (size_s + size_r + size_a + 15) & !15;

    let mut this = Self {
      offsets: HashMap::new(),
      next_offset: Box::new((0..).map(move |i| size_a + i * 4)),
      size_r: size_r,
      frame_size: size,

      labels: HashMap::new(),

      func_data: func,
      insts: vec![],
    };

    // PROLOGUE
    this.push_inst(Inst::Addi(Reg::Sp, Reg::Sp, -size));
    if size_r != 0 {
      this.push_inst(Inst::Sw(Reg::Ra, this.frame_size - 4, Reg::Sp));
    }

    Ok(this)
  }

  pub fn push_inst(&mut self, inst: Inst) {
    self.insts.push(format!("  {}", inst));
  }

  pub fn set_args(&mut self, args: &[Value]) -> Result<()> {
    for (&arg, &reg) in args.iter().zip(CALL_REGS.iter()) {
      let mut rd = reg;
      self.load_value_to_reg(arg, &mut rd)?;
      if rd != reg {
        self.push_inst(Inst::Add(rd, Reg::Zero, reg));
      }
    }
    if args.len() > 8 {
      for (i, &arg) in args[8..].iter().enumerate() {
        self.load_value_to_reg(arg, &mut Reg::T0)?;
        self.push_inst(Inst::Sw(Reg::T0, i as i32 * 4, Reg::Sp));
      }
    }
    Ok(())
  }

  pub fn generate_epilogue(&mut self) {
    // EPILOGUE
    if self.size_r != 0 {
      self.push_inst(Inst::Lw(Reg::Ra, self.frame_size - 4, Reg::Sp));
    }

    self.push_inst(Inst::Addi(Reg::Sp, Reg::Sp, self.frame_size));
    self.push_inst(Inst::Ret);
  }

  /// 获取局部变量所在的内存位置（距离栈指针的偏差）
  pub fn get_offset(&mut self, value: Value) -> Result<i32> {
    if let Some(&offset) = self.offsets.get(&value) {
      Ok(offset)
    } else {
      let offset = self.next_offset.next().unwrap();
      self.offsets.insert(value, offset);
      Ok(offset)
    }
  }

  /// 获取 bb 对应的标签
  pub fn get_label(&self, bb: BasicBlock) -> Result<String> {
    let label = self
      .labels
      .get(&bb)
      .ok_or_else(|| LabelNotExistError(self.func_data.dfg().bb(bb).name().clone().unwrap()))?;
    Ok(label.clone())
  }

  /// 将 Value 加载到寄存器；必要时修改目标寄存器
  pub fn load_value_to_reg(&mut self, value: Value, reg: &mut Reg) -> Result<()> {
    let kind = self.func_data.dfg().value(value).kind();
    if let ValueKind::Integer(integer) = kind {
      // Alloc a register for storing a integer.
      let integer = integer.value();
      if integer != 0 {
        // For non-zero value, use a temp register, then `li` the immediate into it.
        self.push_inst(Inst::Li(*reg, integer));
      } else {
        // For zero, use `zero` register.
        *reg = Reg::Zero;
      }
    } else if let ValueKind::FuncArgRef(arg_ref) = kind {
      // Load function param, from reg or stack.
      let i = arg_ref.index();
      if i < 8 {
        *reg = CALL_REGS[i];
      } else {
        let offset = self.frame_size + (i - 8) as i32 * 4;
        self.push_inst(Inst::Lw(*reg, offset, Reg::Sp));
      }
    } else {
      // Load local variable from stack.
      let offset = self.get_offset(value)?;
      self.push_inst(Inst::Lw(*reg, offset, Reg::Sp));
    }
    Ok(())
  }

  /// 将 Value 保存到寄存器
  pub fn save_value_from_reg(&mut self, value: Value, reg: Reg) -> Result<()> {
    let offset = self.get_offset(value)?;
    self.push_inst(Inst::Sw(reg, offset, Reg::Sp));
    Ok(())
  }
}

pub fn generate(func_name: &str, func_data: &FunctionData) -> Result<Vec<String>> {
  let mut asm: Vec<String> = vec![format!("{}:", func_name)];

  let mut context = GenerateContext::from(&func_data)?;

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
