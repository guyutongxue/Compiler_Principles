mod error;
mod inst;
mod reg;

use koopa::ir::{BinaryOp, FunctionData, Program, Value, ValueKind};
use std::{collections::HashMap, error::Error};

use self::error::*;
use self::inst::Inst;
use self::reg::Reg;

static TEMP_REGS: &[Reg] = &[
  Reg::T0,
  Reg::T1,
  Reg::T2,
  Reg::T3,
  Reg::T4,
  Reg::T5,
  Reg::T6,
  Reg::A0,
  Reg::A1,
  Reg::A2,
  Reg::A3,
  Reg::A4,
  Reg::A5,
  Reg::A6,
  Reg::A7,
];

struct GenerateAsmContext<'a> {
  regs: HashMap<Value, Reg>,
  next_reg: core::slice::Iter<'static, Reg>,
  func_data: &'a FunctionData,
  insts: Vec<String>,
}

impl<'a> GenerateAsmContext<'a> {
  fn from(func: &'a FunctionData) -> Self {
    Self {
      regs: HashMap::new(),
      next_reg: TEMP_REGS.iter(),
      func_data: func,
      insts: vec![],
    }
  }

  fn push_inst(&mut self, inst: Inst) {
    self.insts.push(inst.to_string());
  }

  fn get_reg_from_value(&mut self, value: Value) -> Result<Reg, Box<dyn Error>> {
    if let ValueKind::Integer(integer) = self.func_data.dfg().value(value).kind() {
      if integer.value() == 0 {
        return Ok(Reg::Zero);
      }
    }
    if let Some(reg) = self.regs.get(&value) {
      return Ok(*reg);
    }
    let reg = self.next_reg.next().ok_or(OutOfRegistersError)?;
    self.regs.insert(value, *reg);
    Ok(*reg)
  }
}

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
        fn_asms.push(format!("{}_{}:", func_name, bb_name));
        let mut context = GenerateAsmContext::from(&func_data);
        node.insts().keys().last().unwrap().generate(&mut context)?;
        fn_asms.extend(context.insts.iter().map(|s| format!("  {}", s)));
      }

      text_asms.push(fn_asms.join("\n") + "\n");
    }
    Ok(decls.join("\n") + "\n" + &text_asms.join("\n"))
  }
}

trait GenerateAsmDetail {
  fn generate(self, context: &mut GenerateAsmContext) -> Result<(), Box<dyn Error>>;
}

impl GenerateAsmDetail for Value {
  fn generate(self, context: &mut GenerateAsmContext) -> Result<(), Box<dyn Error>> {
    let data = context.func_data.dfg().value(self);
    match data.kind() {
      ValueKind::Binary(binary) => {
        let lhs = binary.lhs();
        lhs.generate(context)?;
        let lhs_reg = context.get_reg_from_value(lhs)?;
        let rhs = binary.rhs();
        rhs.generate(context)?;
        let rhs_reg = context.get_reg_from_value(rhs)?;
        match binary.op() {
          BinaryOp::Eq => {
            context.push_inst(Inst::Xor(lhs_reg, lhs_reg, rhs_reg));
            context.push_inst(Inst::Seqz(lhs_reg, lhs_reg));
            context.regs.insert(self, lhs_reg);
          }
          BinaryOp::Add => {
            let rd = context.get_reg_from_value(self)?;
            context.push_inst(Inst::Add(rd, lhs_reg, rhs_reg));
          }
          BinaryOp::Sub => {
            let rd = context.get_reg_from_value(self)?;
            context.push_inst(Inst::Sub(rd, lhs_reg, rhs_reg));
          }
          BinaryOp::Mul => {
            let rd = context.get_reg_from_value(self)?;
            context.push_inst(Inst::Mul(rd, lhs_reg, rhs_reg));
          }
          BinaryOp::Div => {
            let rd = context.get_reg_from_value(self)?;
            context.push_inst(Inst::Div(rd, lhs_reg, rhs_reg));
          }
          x => return Err(UnimplementedError(Box::from(x)).into()),
        }
      }
      ValueKind::Integer(integer) => {
        let value = integer.value();
        let rd = context.get_reg_from_value(self)?;
        if value != 0 {
          context.push_inst(Inst::Li(rd, value));
        }
      }

      ValueKind::Return(ret) => {
        let retval = ret.value().ok_or(OtherError("no return value"))?;
        retval.generate(context)?;
        let rs = context.get_reg_from_value(retval)?;
        context.push_inst(Inst::Mv(Reg::A0, rs));
        context.push_inst(Inst::Ret);
      }
      x => return Err(UnimplementedError(Box::from(x.clone())).into()),
    }
    Ok(())
  }
}
