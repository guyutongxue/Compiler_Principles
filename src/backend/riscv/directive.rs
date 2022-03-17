use std::fmt;

#[derive(Debug, Clone)]
pub enum Directive {
  Text,
  Globl(String),
  Data,
  Zero(i32),
  Word(Vec<i32>),
}

impl fmt::Display for Directive {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let str = match self {
      Directive::Text => "  .text".into(),
      Directive::Globl(label) => format!("  .globl {}", label),
      Directive::Data => "  .data".into(),
      Directive::Zero(number) => format!("  .zero {}", number),
      Directive::Word(data) => {
        let data: Vec<_> = data.iter().map(i32::to_string).collect();
        format!("  .word {}", data.join(", "))
      }
    };
    write!(f, "{}", str)
  }
}
