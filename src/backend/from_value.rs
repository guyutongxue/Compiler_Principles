use koopa::ir::{BasicBlock, BinaryOp, Value, ValueKind};
use std::error::Error;

use super::error::*;
use super::from_func::GenerateContext;
use super::riscv::inst::Inst;
use super::riscv::reg::Reg;

pub fn generate(v: Value, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
  v.generate(context)
}

impl<'a> GenerateContext<'a> {
  fn push_inst(&mut self, inst: Inst) {
    self.insts.push(format!("  {}", inst));
  }

  fn get_offset(&mut self, value: Value) -> Result<i32, Box<dyn Error>> {
    if let Some(&offset) = self.offsets.get(&value) {
      Ok(offset)
    } else {
      let offset = self.next_offset.next().unwrap();
      self.offsets.insert(value, offset);
      Ok(offset)
    }
  }

  fn load_value_to_reg(&mut self, value: Value, reg: &mut Reg) -> Result<(), Box<dyn Error>> {
    if let ValueKind::Integer(integer) = self.func_data.dfg().value(value).kind() {
      // Alloc a register for storing a integer.
      let integer = integer.value();
      if integer != 0 {
        // For non-zero value, use a temp register, then `li` the immediate into it.
        self.push_inst(Inst::Li(*reg, integer));
      } else {
        // For zero, use `zero` register.
        *reg = Reg::Zero;
      }
    } else {
      // Load the value from stack.
      let offset = self.get_offset(value)?;
      self.push_inst(Inst::Lw(*reg, offset, Reg::Sp));
    }
    Ok(())
  }

  fn save_value_from_reg(&mut self, value: Value, reg: Reg) -> Result<(), Box<dyn Error>> {
    let offset = self.get_offset(value)?;
    self.push_inst(Inst::Sw(reg, offset, Reg::Sp));
    Ok(())
  }

  pub fn get_label(&self, bb: BasicBlock) -> Result<String, Box<dyn Error>> {
    let label = self
      .labels
      .get(&bb)
      .ok_or_else(|| LabelNotExistError(self.func_data.dfg().bb(bb).name().clone().unwrap()))?;
    Ok(label.clone())
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
        let mut rs1 = Reg::T0;
        context.load_value_to_reg(lhs, &mut rs1)?;
        let rhs = binary.rhs();
        let mut rs2 = Reg::T1;
        context.load_value_to_reg(rhs, &mut rs2)?;
        let rd = Reg::T2;
        match binary.op() {
          BinaryOp::And => {
            context.push_inst(Inst::And(rd, rs1, rs2));
          }
          BinaryOp::Or => {
            context.push_inst(Inst::Or(rd, rs1, rs2));
          }
          BinaryOp::Eq => {
            context.push_inst(Inst::Xor(rd, rs1, rs2));
            context.push_inst(Inst::Seqz(rd, rd));
          }
          BinaryOp::NotEq => {
            context.push_inst(Inst::Xor(rd, rs1, rs2));
            context.push_inst(Inst::Snez(rd, rd));
          }
          BinaryOp::Lt => {
            context.push_inst(Inst::Slt(rd, rs1, rs2));
          }
          BinaryOp::Gt => {
            context.push_inst(Inst::Sgt(rd, rs1, rs2));
          }
          BinaryOp::Le => {
            context.push_inst(Inst::Sgt(rd, rs1, rs2));
            context.push_inst(Inst::Seqz(rd, rd));
          }
          BinaryOp::Ge => {
            context.push_inst(Inst::Slt(rd, rs1, rs2));
            context.push_inst(Inst::Seqz(rd, rd));
          }
          BinaryOp::Add => {
            context.push_inst(Inst::Add(rd, rs1, rs2));
          }
          BinaryOp::Sub => {
            context.push_inst(Inst::Sub(rd, rs1, rs2));
          }
          BinaryOp::Mul => {
            context.push_inst(Inst::Mul(rd, rs1, rs2));
          }
          BinaryOp::Div => {
            context.push_inst(Inst::Div(rd, rs1, rs2));
          }
          BinaryOp::Mod => {
            context.push_inst(Inst::Rem(rd, rs1, rs2));
          }
          x => return Err(UnimplementedError(Box::from(x)).into()),
        }
        context.save_value_from_reg(self, rd)?;
      }
      ValueKind::Return(ret) => {
        let retval = ret.value().ok_or("no return value")?;
        let mut rs = Reg::A0;
        context.load_value_to_reg(retval, &mut rs)?;
        if rs != Reg::A0 {
          context.push_inst(Inst::Mv(Reg::A0, rs));
        }

        // EPILOGUE
        context.push_inst(Inst::Addi(Reg::Sp, Reg::Sp, context.frame_size));

        context.push_inst(Inst::Ret);
      }
      ValueKind::Alloc(_) => {
        context.get_offset(self)?;
      }
      ValueKind::Store(store) => {
        let mut reg = Reg::T0;
        let value = store.value();
        let dest = store.dest();
        context.load_value_to_reg(value, &mut reg)?;
        context.save_value_from_reg(dest, reg)?;
      }
      ValueKind::Load(load) => {
        let mut reg = Reg::T0;
        let src = load.src();
        context.load_value_to_reg(src, &mut reg)?;
        context.save_value_from_reg(self, reg)?;
      }
      ValueKind::Branch(branch) => {
        let cond = branch.cond();
        let mut rd = Reg::T0;
        context.load_value_to_reg(cond, &mut rd)?;
        let true_bb = branch.true_bb();
        let true_label = context.get_label(true_bb)?;
        context.push_inst(Inst::Bnez(rd, true_label));
        let false_bb = branch.false_bb();
        let false_label = context.get_label(false_bb)?;
        context.push_inst(Inst::J(false_label));
      }
      ValueKind::Jump(jump) => {
        let bb = jump.target();
        let label = context.get_label(bb)?;
        context.push_inst(Inst::J(label));
      }
      x => return Err(UnimplementedError(Box::from(x.clone())).into()),
    }
    Ok(())
  }
}
