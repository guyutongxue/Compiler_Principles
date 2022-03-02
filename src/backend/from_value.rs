use koopa::ir::{BinaryOp, TypeKind, Value, ValueKind};

use super::from_func::GenerateContext;
use super::riscv::inst::Inst;
use super::riscv::reg::Reg;
use super::{error::*, DEBUG_INFO, FUNC_NAMES};
use crate::Result;

pub fn generate(value: Value, context: &mut GenerateContext) -> Result<()> {
  context.insts.push(DEBUG_INFO.write()?.pop_front().unwrap());
  match context.value_kind(value) {
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
      context.save_value_from_reg(value, rd)?;
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
      let offset = context.get_local(value);
      context.push_inst(Inst::Addi(Reg::T2, Reg::Sp, offset));
      context.save_value_from_reg(value, Reg::T2)?;
    }
    ValueKind::Store(store) => {
      let value = store.value();
      let mut rs = Reg::T0;
      context.load_value_to_reg(value, &mut rs)?;
      let dest = store.dest();
      if let Some(var) = context.is_global_value(dest)? {
        context.push_inst(Inst::La(Reg::T3, var));
        context.push_inst(Inst::Sw(rs, 0, Reg::T3));
      } else {
        let mut rd = Reg::T1;
        context.load_value_to_reg(dest, &mut rd)?;
        context.push_inst(Inst::Sw(rs, 0, rd));
      }
    }
    ValueKind::Load(load) => {
      let rd = Reg::T2;
      let mut rs = Reg::T0;
      let src = load.src();
      if let Some(var) = context.is_global_value(src)? {
        context.push_inst(Inst::La(rs, var));
      } else {
        context.load_value_to_reg(src, &mut rs)?;
      }
      context.push_inst(Inst::Lw(rd, 0, rs));
      context.save_value_from_reg(value, rd)?;
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
      if context.value_type(value).is_i32() {
        context.save_value_from_reg(value, Reg::A0)?;
      }
    }
    ValueKind::GetElemPtr(elem_ptr) => {
      let base = elem_ptr.src();
      let index = elem_ptr.index();
      let step = if let TypeKind::Pointer(base) = context.value_type(value).kind() {
        base.size()
      } else {
        panic!("getelemptr should be pointer type")
      };
      let result = generate_get_ptr(context, base, index, step as i32)?;
      context.save_value_from_reg(value, result)?;
    }
    ValueKind::GetPtr(ptr) => {
      let base = ptr.src();
      let index = ptr.index();
      let step = if let TypeKind::Pointer(base) = context.value_type(value).kind() {
        base.size()
      } else {
        panic!("getelemptr should be pointer type")
      };
      let result = generate_get_ptr(context, base, index, step as i32)?;
      context.save_value_from_reg(value, result)?;
    }
    x => return Err(UnimplementedError(Box::from(x.clone())).into()),
  }
  Ok(())
}

fn generate_get_ptr(
  context: &mut GenerateContext,
  base: Value,
  index: Value,
  step: i32,
) -> Result<Reg> {
  let mut base_reg = Reg::T0;
  if let Some(var) = context.is_global_value(base)? {
    context.push_inst(Inst::La(base_reg, var));
  } else {
    context.load_value_to_reg(base, &mut base_reg)?;
  }

  let mut index_reg = Reg::T1;
  context.load_value_to_reg(index, &mut index_reg)?;
  context.push_inst(Inst::Li(Reg::T2, step as i32));
  context.push_inst(Inst::Mul(Reg::T1, index_reg, Reg::T2));

  context.push_inst(Inst::Add(base_reg, base_reg, Reg::T1));
  Ok(base_reg)
}
