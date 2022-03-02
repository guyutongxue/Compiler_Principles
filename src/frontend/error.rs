use std::fmt;

use std::error::Error;

#[derive(Debug)]
pub struct UnimplementedError(pub String);

impl Error for UnimplementedError {}

impl fmt::Display for UnimplementedError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{} unimplemented", self.0)
  }
}

#[derive(Debug)]
pub enum CompileError{
  UndeclaredSymbol(String),
  TypeMismatch(&'static str, String, &'static str),
  IllegalBreak,
  IllegalContinue,
  IllegalVoid,
  Redefinition(String),
  ConstexprRequired(&'static str),
  NegativeSubscript(i32),
  IndexOutOfBounds(i32, usize),
  InitializerRequired(String),
  TooManyInitializers,
  Other(String),
}

impl CompileError {
  fn message(&self) -> String {
    match self {
      Self::UndeclaredSymbol(ident) => format!("符号 '{}' 未定义", ident),
      Self::TypeMismatch(expect, val, now) => format!("类型不匹配： 期望{}类型，但 '{}' 是{}类型", expect, val, now),
      Self::IllegalBreak => "break 只能在循环中使用".into(),
      Self::IllegalContinue => "continue 只能在循环中使用".into(),
      Self::IllegalVoid => "不能将变量声明为 void 类型".into(),
      Self::Redefinition(ident) => format!("符号 '{}' 重复定义", ident),
      Self::ConstexprRequired(ty) => format!("{}必须是常量表达式", ty),
      Self::NegativeSubscript(val) => format!("不能用负数 {} 作为数组大小", val),
      Self::IndexOutOfBounds(val, lim) => format!("数组索引 {} 超出范围 [0, {})", val, lim),
      Self::InitializerRequired(val) => format!("常量 {} 的声明需带初始化器", val),
      Self::TooManyInitializers => "初始化器太多".into(),
      Self::Other(msg) => msg.clone(),
    }
  }
}

impl Error for CompileError {}

impl fmt::Display for CompileError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "\x1b[0;31m编译错误\x1b[0m {}", self.message())
  }
}

#[derive(Debug)]
pub struct PushKeyError(pub Box<dyn fmt::Debug>);

impl Error for PushKeyError {}

impl fmt::Display for PushKeyError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "key {:#?} already exists", self.0)
  }
}
