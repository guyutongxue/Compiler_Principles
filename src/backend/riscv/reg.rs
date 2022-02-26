// https://pku-minic.github.io/online-doc/#/misc-app-ref/riscv-insts

use std::fmt;

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Reg {
  /// 恒为 0。
  Zero,
  /// 返回地址。调用者保存。
  Ra,
  /// 栈指针。被调用者保存。
  Sp,
  /// 全局指针。
  Gp,
  /// 线程指针。
  Tp,
  /// 临时/备用链接寄存器。调用者保存。
  T0,
  /// 临时寄存器。调用者保存。
  T1,
  /// 临时寄存器。调用者保存。
  T2,
  /// 保存寄存器/帧指针。被调用者保存。
  Fp,
  /// 保存寄存器。被调用者保存。
  S1,
  /// 函数参数/返回值。调用者保存。
  A0,
  /// 函数参数/返回值。调用者保存。
  A1,
  /// 函数参数。调用者保存。
  A2,
  /// 函数参数。调用者保存。
  A3,
  /// 函数参数。调用者保存。
  A4,
  /// 函数参数。调用者保存。
  A5,
  /// 函数参数。调用者保存。
  A6,
  /// 函数参数。调用者保存。
  A7,
  /// 保存寄存器。被调用者保存。
  S2,
  /// 保存寄存器。被调用者保存。
  S3,
  /// 保存寄存器。被调用者保存。
  S4,
  /// 保存寄存器。被调用者保存。
  S5,
  /// 保存寄存器。被调用者保存。
  S6,
  /// 保存寄存器。被调用者保存。
  S7,
  /// 保存寄存器。被调用者保存。
  S8,
  /// 保存寄存器。被调用者保存。
  S9,
  /// 保存寄存器。被调用者保存。
  S10,
  /// 保存寄存器。被调用者保存。
  S11,
  /// 临时寄存器。调用者保存。
  T3,
  /// 临时寄存器。调用者保存。
  T4,
  /// 临时寄存器。调用者保存。
  T5,
  /// 临时寄存器。调用者保存。
  T6,
}

impl Reg {
  fn as_str(&self) -> &'static str {
    match self {
      Reg::Zero => "zero",
      Reg::Ra => "ra",
      Reg::Sp => "sp",
      Reg::Gp => "gp",
      Reg::Tp => "tp",
      Reg::T0 => "t0",
      Reg::T1 => "t1",
      Reg::T2 => "t2",
      Reg::Fp => "fp",
      Reg::S1 => "s1",
      Reg::A0 => "a0",
      Reg::A1 => "a1",
      Reg::A2 => "a2",
      Reg::A3 => "a3",
      Reg::A4 => "a4",
      Reg::A5 => "a5",
      Reg::A6 => "a6",
      Reg::A7 => "a7",
      Reg::S2 => "s2",
      Reg::S3 => "s3",
      Reg::S4 => "s4",
      Reg::S5 => "s5",
      Reg::S6 => "s6",
      Reg::S7 => "s7",
      Reg::S8 => "s8",
      Reg::S9 => "s9",
      Reg::S10 => "s10",
      Reg::S11 => "s11",
      Reg::T3 => "t3",
      Reg::T4 => "t4",
      Reg::T5 => "t5",
      Reg::T6 => "t6",
    }
  }
}

impl fmt::Display for Reg {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.as_str())
  }
}
