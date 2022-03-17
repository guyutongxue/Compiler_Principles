use crate::backend::riscv::{Riscv, RiscvItem, inst::Inst};

pub fn pass_peephole(old: &Riscv) -> Riscv {
  let old: Vec<_> = old
    .0
    .iter()
    .filter(|item| !matches!(item, RiscvItem::Comment(_)))
    .collect();

  let mut result = vec![];

  let mut i = 0;
  while i < old.len() - 1 {
    result.push(old[i].clone());
    if let RiscvItem::Inst(Inst::Sw(s_rs2, s_imm, s_rs1)) = old[i] {
      if let RiscvItem::Inst(Inst::Lw(l_rs, l_imm, l_rd)) = old[i + 1] {
        if l_rd == s_rs1 && l_imm == s_imm {
          if s_rs2 != l_rs {
            result.push(RiscvItem::Inst(Inst::Addi(*l_rs, *s_rs2, 0)));
          }
          i += 2;
          continue;
        }
      }
    }
    i += 1;
  }

  return Riscv(result);
}
