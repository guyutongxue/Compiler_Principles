use std::cmp;
use std::collections::HashMap;

use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{BasicBlock, Function, Program, Type, TypeKind, Value, ValueKind};

use super::error::LabelNotExistError;
use super::from_value;
use super::riscv::{inst::Inst, reg::Reg};
use super::{DEBUG_INFO, FUNC_NAMES, VAR_NAMES};
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
  /// 局部变量（Alloc）到内存位置的映射
  pub locals: HashMap<Value, i32>,
  /// 计算结果（IR Value）到内存位置（距离栈指针偏差）的映射
  temps: HashMap<Value, i32>,
  size_a: i32,
  size_r: i32,
  /// 栈帧大小
  pub frame_size: i32,

  /// IR 基本块到汇编标签的映射
  pub labels: HashMap<BasicBlock, String>,

  /// 已生成指令序列
  pub insts: Vec<String>,

  /// IR 数据
  pub program: &'a Program,
  pub func: Function,
}

impl<'a> GenerateContext<'a> {
  fn from(prog: &'a Program, func: Function) -> Result<Self> {
    // 分配局部变量空间
    let mut locals = HashMap::new();
    let mut local_size = 0;
    let bbs = prog.func(func).layout().bbs();
    for (_, node) in bbs {
      for (&v, _) in node.insts() {
        if let ValueKind::Alloc(_) = prog.func(func).dfg().value(v).kind() {
          locals.insert(v, local_size);
          if let TypeKind::Pointer(base) = prog.func(func).dfg().value(v).ty().kind() {
            local_size += base.size() as i32;
          } else {
            panic!("alloc do not have pointer type");
          };
        }
      }
    }

    // 分配计算结果空间
    let mut temps = HashMap::new();
    let mut temp_size = local_size;
    for (_, node) in bbs {
      for (&v, _) in node.insts() {
        temps.insert(v, temp_size);
        temp_size += prog.func(func).dfg().value(v).ty().size() as i32;
      }
    }

    let calls: Vec<_> = prog
      .func(func)
      .dfg()
      .values()
      .iter()
      .filter_map(|(_, vd)| match vd.kind() {
        ValueKind::Call(func) => Some(func.args().len() as i32),
        _ => None,
      })
      .collect();

    let size_s = temp_size;
    let size_r = if calls.len() > 0 { 4 } else { 0 };
    let size_a = calls
      .iter()
      .map(|len| cmp::max(len - 8, 0))
      .max()
      .unwrap_or(0)
      * 4;

    let size = (size_s + size_r + size_a + 15) & !15;

    let mut this = Self {
      locals,
      temps,
      size_a,
      size_r,
      frame_size: size,
      labels: HashMap::new(),
      insts: vec![],
      program: prog,
      func,
    };

    // PROLOGUE
    this.push_inst(Inst::Addi(Reg::Sp, Reg::Sp, -size));
    if size_r != 0 {
      this.push_inst(Inst::Sw(Reg::Ra, this.frame_size - 4, Reg::Sp));
    }

    Ok(this)
  }

  pub fn dfg(&self) -> &DataFlowGraph {
    self.program.func(self.func).dfg()
  }

  pub fn value_kind(&self, v: Value) -> ValueKind {
    self.dfg().value(v).kind().clone()
  }

  pub fn value_type(&self, value: Value) -> Type {
    let val = self.program.func(self.func).dfg().values().get(&value);
    if let Some(val) = val {
      return val.ty().clone();
    }
    self.program.borrow_value(value).ty().clone()
  }

  pub fn is_global_value(&self, v: Value) -> Result<Option<String>> {
    if self.dfg().values().get(&v).is_some() {
      Ok(None)
    } else {
      let var = VAR_NAMES
        .read()?
        .get(&v)
        .cloned()
        .ok_or("global variable not found".to_string())?;
      Ok(Some(var))
    }
  }

  pub fn push_inst(&mut self, inst: Inst) {
    self.insts.push(inst.to_string());
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

  pub fn get_local(&self, v: Value) -> i32 {
    *self.locals.get(&v).expect("Cannot find local var") + self.size_a
  }

  /// 获取所在的内存位置（距离栈指针的偏差）
  pub fn get_offset(&mut self, value: Value) -> Result<i32> {
    if let Some(&offset) = self.temps.get(&value) {
      Ok(offset + self.size_a)
    } else {
      let vd = self.dfg().value(value);
      panic!("Where to store value {:?}?", vd);
    }
  }

  /// 获取 bb 对应的标签
  pub fn get_label(&self, bb: BasicBlock) -> Result<String> {
    let label = self
      .labels
      .get(&bb)
      .ok_or_else(|| LabelNotExistError(self.dfg().bb(bb).name().clone().unwrap()))?;
    Ok(label.clone())
  }

  /// 将 Value 加载到寄存器；必要时修改目标寄存器
  pub fn load_value_to_reg(&mut self, value: Value, reg: &mut Reg) -> Result<()> {
    let kind = self.dfg().value(value).kind();
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

pub fn generate(program: &Program, func: Function) -> Result<Vec<String>> {
  let func_data = program.func(func);
  let func_name = &func_data.name()[1..];
  FUNC_NAMES.write()?.insert(func, func_name.into());
  let func_name = &func_data.name()[1..];

  if func_data.layout().entry_bb().is_none() {
    // Function declaration, skip.
    DEBUG_INFO.write()?.pop_front();
    DEBUG_INFO.write()?.pop_front();
    return Ok(vec![]);
  }

  let mut result = vec![];
  result.push(DEBUG_INFO.write()?.pop_front().unwrap());
  result.push("  .text".into());
  result.push(format!("  .globl {}", func_name));
  result.push(format!("{}:", func_name));
  let mut context = GenerateContext::from(program, func)?;

  // Generate map from BB to label
  for (&bb, _) in func_data.layout().bbs() {
    let bb_name = &func_data.dfg().bb(bb).name().clone().unwrap()[1..];
    let label_name = format!("{}_{}", func_name, bb_name);
    context.labels.insert(bb, label_name.clone());
  }

  for (&bb, node) in func_data.layout().bbs() {
    let label = context.get_label(bb)?;
    result.push(DEBUG_INFO.write()?.pop_front().unwrap());
    result.push(format!("{}:", label));
    for &i in node.insts().keys() {
      from_value::generate(i, &mut context)?;
    }
    result.append(&mut context.insts);
  }
  result.push("".into());
  DEBUG_INFO.write()?.pop_front();
  DEBUG_INFO.write()?.pop_front();

  Ok(result)
}
