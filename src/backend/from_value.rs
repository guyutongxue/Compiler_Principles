use koopa::ir::{BinaryOp, Value, ValueKind};
use std::error::Error;

use super::from_func::GenerateContext;
use super::riscv::inst::Inst;
use super::riscv::reg::Reg;
use super::{error::*, FUNC_NAMES, VAR_NAMES};

pub fn generate(v: Value, context: &mut GenerateContext) -> Result<(), Box<dyn Error>> {
  v.generate(context)
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
        if let Some(retval) = ret.value() {
          let mut rs = Reg::A0;
          context.load_value_to_reg(retval, &mut rs)?;
          if rs != Reg::A0 {
            context.push_inst(Inst::Mv(Reg::A0, rs));
          }
        }
        context.generate_epilogue();
      }
      ValueKind::Alloc(_) => {
        context.get_offset(self)?;
      }
      ValueKind::Store(store) => {
        let mut reg = Reg::T0;
        let value = store.value();
        let dest = store.dest();
        context.load_value_to_reg(value, &mut reg)?;
        if context.func_data.dfg().values().get(&dest).is_some() {
          context.save_value_from_reg(dest, reg)?;
        } else {
          let var_name = VAR_NAMES
            .read()?
            .get(&dest)
            .cloned()
            .ok_or(LabelNotExistError("global variable ??".into()))?;
          context.push_inst(Inst::La(Reg::T3, var_name));
          context.push_inst(Inst::Sw(reg, 0, Reg::T3));
        }
      }
      ValueKind::Load(load) => {
        let mut reg = Reg::T0;
        let src = load.src();
        if context.func_data.dfg().values().get(&src).is_some() {
          context.load_value_to_reg(src, &mut reg)?;
        } else {
          let var_name = VAR_NAMES
            .read()?
            .get(&src)
            .cloned()
            .ok_or(LabelNotExistError("global variable ??".into()))?;
          context.push_inst(Inst::La(reg, var_name));
          context.push_inst(Inst::Lw(reg, 0, reg));
        }
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
      ValueKind::Call(func) => {
        let args = func.args();
        context.set_args(args)?;
        let callee = FUNC_NAMES
          .read()?
          .get(&func.callee())
          .cloned()
          .ok_or(LabelNotExistError("global function ??".into()))?;
        context.push_inst(Inst::Call(callee));
        if data.ty().is_i32() {
          context.save_value_from_reg(self, Reg::A0)?;
        }
      }
      x => return Err(UnimplementedError(Box::from(x.clone())).into()),
    }
    Ok(())
  }
}
