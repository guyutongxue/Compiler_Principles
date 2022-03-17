pub mod inst;
pub mod reg;
pub mod directive;

use std::fmt;

use self::{inst::Inst, directive::Directive};

#[derive(Debug, Clone)]
pub enum RiscvItem {
  Label(String),
  Inst(Inst),
  Comment(String),
  Directive(Directive),
  Empty,
}

pub struct Riscv(pub Vec<RiscvItem>);

impl Riscv {
  pub fn new() -> Self {
    Self(Vec::new())
  }

  pub fn add_label(&mut self, label: String) {
    self.0.push(RiscvItem::Label(label));
  }
  pub fn add_inst(&mut self, inst: Inst) {
    self.0.push(RiscvItem::Inst(inst));
  }
  pub fn add_comment(&mut self, comment: String) {
    self.0.push(RiscvItem::Comment(comment));
  }
  pub fn add_directive(&mut self, directive: Directive) {
    self.0.push(RiscvItem::Directive(directive));
  }
  pub fn add_empty(&mut self) {
    self.0.push(RiscvItem::Empty);
  }

  pub fn extend(&mut self, other: Riscv) {
    self.0.extend(other.0);
  }
  pub fn append(&mut self, other: &mut Riscv) {
    self.0.append(&mut other.0);
  }
}

impl fmt::Display for RiscvItem {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let str = match self {
      RiscvItem::Label(label) => format!("{}:", label),
      RiscvItem::Inst(inst) => inst.to_string(),
      RiscvItem::Comment(comment) => format!("# {}", comment),
      RiscvItem::Directive(directive) => directive.to_string(),
      RiscvItem::Empty => "".into(),
    };
    write!(f, "{}", str)
  }
}

impl fmt::Display for Riscv {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    for item in &self.0 {
      write!(f, "{}\n", item)?;
    }
    Ok(())
  }
}
