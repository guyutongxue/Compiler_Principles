use koopa::ir::{BinaryOp, Value, ValueKind};
use std::error::Error;

use super::from_func::GenerateContext;
use super::error::*;
use super::riscv::inst::Inst;
use super::riscv::reg::Reg;

pub static TEMP_REGS: &[Reg] = &[
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

pub fn generate(v: Value, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
  v.generate(context)
}

impl<'a> GenerateContext<'a> {

  fn push_inst(&mut self, inst: Inst) {
    self.insts.push(inst.to_string());
  }

  fn get_reg_from_value(&mut self, value: Value) -> Result<Reg, Box<dyn Error>> {
    if let Some(reg) = self.regs.get(&value) {
      return Ok(*reg);
    }
    if let ValueKind::Integer(integer) = self.func_data.dfg().value(value).kind() {
      // Alloc a register for storing this integer.
      let integer = integer.value();
      if integer != 0 {
        // For non-zero value, use a temp register, then `li` the immediate into it.
        let reg = *self.next_reg.next().ok_or(OutOfRegistersError)?;
        self.regs.insert(value, reg);
        self.push_inst(Inst::Li(reg, integer));
        Ok(reg)
      } else {
        // For zero, use `zero` register.
        Ok(Reg::Zero)
      }
    } else {
      let reg = *self.next_reg.next().ok_or(OutOfRegistersError)?;
      self.regs.insert(value, reg);
      Ok(reg)
    }
  }
}

trait GenerateAsmDetail {
  fn generate(self, context: &mut GenerateContext) -> Result<(), Box<dyn Error>>;
}

impl GenerateAsmDetail for Value {
  fn generate(self, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
    let data = context.func_data.dfg().value(self);
    match data.kind() {
      ValueKind::Binary(binary) => {
        let lhs = binary.lhs();
        let rs1 = context.get_reg_from_value(lhs)?;
        let rhs = binary.rhs();
        let rs2 = context.get_reg_from_value(rhs)?;
        let rd = if rs2 == Reg::Zero {
          context.get_reg_from_value(self)?
        } else {
          rs2
        };
        match binary.op() {
          BinaryOp::And => {
            context.push_inst(Inst::And(rd, rs1, rs2));
            context.regs.insert(self, rd);
          }
          BinaryOp::Or => {
            context.push_inst(Inst::Or(rd, rs1, rs2));
            context.regs.insert(self, rd);
          }
          BinaryOp::Eq => {
            context.push_inst(Inst::Xor(rd, rs1, rs2));
            context.push_inst(Inst::Seqz(rd, rd));
            context.regs.insert(self, rd);
          }
          BinaryOp::NotEq => {
            context.push_inst(Inst::Xor(rd, rs1, rs2));
            context.push_inst(Inst::Snez(rd, rd));
            context.regs.insert(self, rd);
          }
          BinaryOp::Lt => {
            context.push_inst(Inst::Slt(rd, rs1, rs2));
            context.regs.insert(self, rd);
          }
          BinaryOp::Gt => {
            context.push_inst(Inst::Sgt(rd, rs1, rs2));
            context.regs.insert(self, rd);
          }
          BinaryOp::Le => {
            context.push_inst(Inst::Sgt(rd, rs1, rs2));
            context.push_inst(Inst::Seqz(rd, rd));
            context.regs.insert(self, rd);
          }
          BinaryOp::Ge => {
            context.push_inst(Inst::Slt(rd, rs1, rs2));
            context.push_inst(Inst::Seqz(rd, rd));
            context.regs.insert(self, rd);
          }
          BinaryOp::Add => {
            context.push_inst(Inst::Add(rd, rs1, rs2));
            context.regs.insert(self, rd);
          }
          BinaryOp::Sub => {
            context.push_inst(Inst::Sub(rd, rs1, rs2));
            context.regs.insert(self, rd);
          }
          BinaryOp::Mul => {
            context.push_inst(Inst::Mul(rd, rs1, rs2));
            context.regs.insert(self, rd);
          }
          BinaryOp::Div => {
            context.push_inst(Inst::Div(rd, rs1, rs2));
            context.regs.insert(self, rd);
          }
          BinaryOp::Mod => {
            context.push_inst(Inst::Rem(rd, rs1, rs2));
            context.regs.insert(self, rd);
          }
          x => return Err(UnimplementedError(Box::from(x)).into()),
        }
      }

      ValueKind::Return(ret) => {
        let retval = ret.value().ok_or("no return value")?;
        let rs = context.get_reg_from_value(retval)?;
        context.push_inst(Inst::Mv(Reg::A0, rs));
        context.push_inst(Inst::Ret);
      }
      x => return Err(UnimplementedError(Box::from(x.clone())).into()),
    }
    Ok(())
  }
}
