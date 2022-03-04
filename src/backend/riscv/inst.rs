// https：//pku-minicgithubio/online-doc/#/misc-app-ref/riscv-insts

use std::fmt;

use super::reg::Reg;

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Inst {
  /// 伪指令
  /// - 汇编格式：`beqz rs, label`
  /// - 行为：如果 `rs` 寄存器的值等于 0，则转移到目标 `label`
  Beqz(Reg, String),

  /// 伪指令
  /// - 汇编格式：`bnez rs, label`
  /// - 行为：如果 `rs` 寄存器的值不等于 0，则转移到目标 `label`
  Bnez(Reg, String),

  /// 伪指令
  /// - 汇编格式：`j label`
  /// - 行为：无条件转移到目标 `label`
  J(String),

  /// 伪指令
  /// - 汇编格式：`call label`
  /// - 行为：将后一条指令的地址存入 `ra` 寄存器，并无条件转移到目标 `label`
  Call(String),

  /// 伪指令
  /// - 汇编格式：`ret`
  /// - 行为：无条件转移到 `ra` 寄存器中保存的地址处
  Ret,

  /// 指令
  /// - 汇编格式：`lw rs, imm12(rd)`
  /// - 行为：计算 `rd` 寄存器的值与 `imm12` 相加的结果作为访存地址，从内存中读取 32-bit 的数据，存入 `rs` 寄存器
  Lw(Reg, i32, Reg),

  /// 指令
  /// - 汇编格式：`sw rs2, imm12(rs1)`
  /// - 行为：计算 `rs1` 寄存器的值与 `imm12` 相加的结果作为访存地址，将 `rs2` 寄存器的值 (32-bit) 存入内存
  Sw(Reg, i32, Reg),

  /// 指令
  /// - 汇编格式：`add rd, rs1, rs2`
  /// - 行为：计算 `rs1` 寄存器和 `rs2` 寄存器相加的值，存入 `rd` 寄存器
  Add(Reg, Reg, Reg),

  /// 指令
  /// - 汇编格式：`addi rd, rs1, imm12`
  /// - 行为：计算 `rs1` 寄存器和 `imm12` 相加的值，存入 `rd` 寄存器
  Addi(Reg, Reg, i32),

  /// 指令
  /// - 汇编格式：`sub rd, rs1, rs2`
  /// - 行为：计算 `rs1` 寄存器和 `rs2` 寄存器相减的值，存入 `rd` 寄存器
  Sub(Reg, Reg, Reg),

  /// 指令
  /// - 汇编格式：`slt rd, rs1, rs2`
  /// - 行为：如果 `rs1` 寄存器小于 `rs2` 寄存器，则将 1 写入 `rd` 寄存器，否则写入 0
  Slt(Reg, Reg, Reg),

  /// 伪指令
  /// - 汇编格式：`sgt rd, rs1, rs2`
  /// - 行为：如果 `rs1` 寄存器大于 `rs2` 寄存器，则将 1 写入 `rd` 寄存器，否则写入 0
  Sgt(Reg, Reg, Reg),

  /// 伪指令
  /// - 汇编格式：`seqz rd, rs`
  /// - 行为：如果 `rs` 寄存器等于 0，则将 1 写入 `rd` 寄存器，否则写入 0
  Seqz(Reg, Reg),

  /// 伪指令
  /// - 汇编格式：`snez rd, rs`
  /// - 行为：如果 `rs` 寄存器不等于 0，则将 1 写入 `rd` 寄存器，否则写入 0
  Snez(Reg, Reg),

  /// 指令
  /// - 汇编格式：`xor rd, rs1, rs2`
  /// - 行为：计算 `rs1` 寄存器和 `rs2` 寄存器按位异或的值，存入 `rd` 寄存器
  Xor(Reg, Reg, Reg),

  /// 指令
  /// - 汇编格式：`xori rd, rs1, imm12`
  /// - 行为：计算 `rs1` 寄存器和 `imm12` 按位异或的值，存入 `rd` 寄存器
  Xori(Reg, Reg, i32),

  /// 指令
  /// - 汇编格式：`or rd, rs1, rs2`
  /// - 行为：计算 `rs1` 寄存器和 `rs2` 寄存器按位或的值，存入 `rd` 寄存器
  Or(Reg, Reg, Reg),

  /// 指令
  /// - 汇编格式：`ori rd, rs1, imm12`
  /// - 行为：计算 `rs1` 寄存器和 `imm12` 按位或的值，存入 `rd` 寄存器
  Ori(Reg, Reg, i32),

  /// 指令
  /// - 汇编格式：`and rd, rs1, rs2`
  /// - 行为：计算 `rs1` 寄存器和 `rs2` 寄存器按位与的值，存入 `rd` 寄存器
  And(Reg, Reg, Reg),

  /// 指令
  /// - 汇编格式：`andi rd, rs1, imm12`
  /// - 行为：计算 `rs1` 寄存器和 `imm12` 按位与的值，存入 `rd` 寄存器
  Andi(Reg, Reg, i32),

  /// 指令
  /// - 汇编格式：`sll rd, rs1, rs2`
  /// - 行为：对寄存器 `rs1` 进行逻辑左移运算，移位的位数为 `rs2` 寄存器的值，结果存入 `rd` 寄存器
  Sll(Reg, Reg, Reg),

  /// 指令
  /// - 汇编格式：`srl rd, rs1, rs2`
  /// - 行为：对寄存器 `rs1` 进行逻辑右移运算，移位的位数为 `rs2` 寄存器的值，结果存入 `rd` 寄存器
  Srl(Reg, Reg, Reg),

  /// 指令
  /// - 汇编格式：`sra rd, rs1, rs2`
  /// - 行为：对寄存器 `rs1` 进行算数右移运算，移位的位数为 `rs2` 寄存器的值，结果存入 `rd` 寄存器
  Sra(Reg, Reg, Reg),

  /// 指令
  /// - 汇编格式：`mul rd, rs1, rs2`
  /// - 行为：计算寄存器 `rs1` 和寄存器 `rs2` 相乘的值，存入 `rd` 寄存器
  Mul(Reg, Reg, Reg),

  /// 指令
  /// - 汇编格式：`div rd, rs1, rs2`
  /// - 行为：计算寄存器 `rs1` 和寄存器 `rs2` 相除以的值，存入 `rd` 寄存器
  Div(Reg, Reg, Reg),

  /// 指令
  /// - 汇编格式：`rem rd, rs1, rs2`
  /// - 行为：计算寄存器 `rs1` 和寄存器 `rs2` 相取余的值，存入 `rd` 寄存器
  Rem(Reg, Reg, Reg),

  /// 伪指令
  /// - 汇编格式：`li rd, imm`
  /// - 行为：将立即数 `imm` 加载到寄存器 `rd` 中
  Li(Reg, i32),

  /// 伪指令
  /// - 汇编格式：`la rd, label`
  /// - 行为：将标号 `label` 的绝对地址加载到寄存器 `rd` 中
  La(Reg, String),

  /// 伪指令
  /// - 汇编格式：`mv rd, rs`
  /// - 行为：将寄存器 `rs` 的值复制到寄存器 `rd`
  Mv(Reg, Reg),
}

fn fmt_reg2(name: &str, reg1: Reg, reg2: Reg) -> String {
  format!("  {} {}, {}", name, reg1, reg2)
}

fn fmt_reg3(name: &str, reg1: Reg, reg2: Reg, reg3: Reg) -> String {
  format!("  {} {}, {}, {}", name, reg1, reg2, reg3)
}

fn fmt_reg2_offset(name: &str, reg1: Reg, reg2: Reg, offset: i32) -> String {
  if offset < -2048 || offset > 2047 {
    format!(
      "  li t6, {}\n  add t6, t6, {}\n  {} {}, 0(t6)",
      offset, reg2, name, reg1
    )
  } else {
    format!("  {} {}, {}({})", name, reg1, offset, reg2)
  }
}

fn fmt_reg2_imm(name: &str, reg1: Reg, reg2: Reg, imm: i32) -> String {
  if imm < -2048 || imm > 2047 {
    let len = name.len();
    format!(
      "  li t6, {}\n  {} {}, {}, t6",
      imm,
      &name[..len - 1],
      reg1,
      reg2
    )
  } else {
    format!("  {} {}, {}, {}", name, reg1, reg2, imm)
  }
}

fn fmt_reg_label(name: &str, reg: Reg, label: &String) -> String {
  format!("  {} {}, {}", name, reg, label)
}

fn fmt_label(name: &str, label: &String) -> String {
  format!("  {} {}", name, label)
}

fn fmt_reg_imm(name: &str, reg: Reg, imm: i32) -> String {
  format!("  {} {}, {}", name, reg, imm)
}

impl fmt::Display for Inst {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let inst = match self {
      Inst::Beqz(rs, label) => fmt_reg_label("beqz", *rs, label),
      Inst::Bnez(rs, label) => fmt_reg_label("bnez", *rs, label),
      Inst::J(label) => fmt_label("j", label),
      Inst::Call(label) => fmt_label("call", label),
      Inst::Ret => "  ret".into(),
      Inst::Lw(rd, offset, rs) => fmt_reg2_offset("lw", *rd, *rs, *offset),
      Inst::Sw(rd, offset, rs) => fmt_reg2_offset("sw", *rd, *rs, *offset),
      Inst::Add(rd, rs1, rs2) => fmt_reg3("add", *rd, *rs1, *rs2),
      Inst::Addi(rd, rs, imm) => fmt_reg2_imm("addi", *rd, *rs, *imm),
      Inst::Sub(rd, rs1, rs2) => fmt_reg3("sub", *rd, *rs1, *rs2),
      Inst::Slt(rd, rs1, rs2) => fmt_reg3("slt", *rd, *rs1, *rs2),
      Inst::Sgt(rd, rs1, rs2) => fmt_reg3("sgt", *rd, *rs1, *rs2),
      Inst::Seqz(rd, rs) => fmt_reg2("seqz", *rd, *rs),
      Inst::Snez(rd, rs) => fmt_reg2("snez", *rd, *rs),
      Inst::Xor(rd, rs1, rs2) => fmt_reg3("xor", *rd, *rs1, *rs2),
      Inst::Xori(rd, rs, imm) => fmt_reg2_imm("xori", *rd, *rs, *imm),
      Inst::Or(rd, rs1, rs2) => fmt_reg3("or", *rd, *rs1, *rs2),
      Inst::Ori(rd, rs, imm) => fmt_reg2_imm("ori", *rd, *rs, *imm),
      Inst::And(rd, rs1, rs2) => fmt_reg3("and", *rd, *rs1, *rs2),
      Inst::Andi(rd, rs, imm) => fmt_reg2_imm("andi", *rd, *rs, *imm),
      Inst::Sll(rd, rs1, rs2) => fmt_reg3("sll", *rd, *rs1, *rs2),
      Inst::Srl(rd, rs1, rs2) => fmt_reg3("srl", *rd, *rs1, *rs2),
      Inst::Sra(rd, rs1, rs2) => fmt_reg3("sra", *rd, *rs1, *rs2),
      Inst::Mul(rd, rs1, rs2) => fmt_reg3("mul", *rd, *rs1, *rs2),
      Inst::Div(rd, rs1, rs2) => fmt_reg3("div", *rd, *rs1, *rs2),
      Inst::Rem(rd, rs1, rs2) => fmt_reg3("rem", *rd, *rs1, *rs2),
      Inst::Li(rd, imm) => fmt_reg_imm("li", *rd, *imm),
      Inst::La(rd, label) => fmt_reg_label("la", *rd, label),
      Inst::Mv(rd, rs) => fmt_reg2("mv", *rd, *rs),
    };
    write!(f, "{}", inst)
  }
}
