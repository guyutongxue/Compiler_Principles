use std::fmt;

use std::error::Error;

use lalrpop_util::ParseError;
use lalrpop_util::lexer::Token;

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
  CanNotAssign(String, &'static str),
  IllegalBreak,
  IllegalContinue,
  IllegalVoid,
  Redefinition(String),
  ConstexprRequired(&'static str),
  NegativeSubscript(i32, &'static str),
  InitializerRequired(String),
  Other(String),
}

impl CompileError {
  fn message(&self) -> String {
    match self {
      Self::UndeclaredSymbol(ident) => format!("符号 '{}' 未定义", ident),
      Self::TypeMismatch(expect, val, now) => format!("类型不匹配： 期望{}类型，但 '{}' 是{}类型", expect, val, now),
      Self::CanNotAssign(ident, ty) => format!("'{}' 是{}，不可被赋值", ident, ty),
      Self::IllegalBreak => "break 只能在循环中使用".into(),
      Self::IllegalContinue => "continue 只能在循环中使用".into(),
      Self::IllegalVoid => "不能将变量声明为 void 类型".into(),
      Self::Redefinition(ident) => format!("符号 '{}' 重复定义", ident),
      Self::ConstexprRequired(ty) => format!("{}必须是常量表达式", ty),
      Self::NegativeSubscript(val, ty) => format!("不能用负数 {} {}", val, ty),
      Self::InitializerRequired(val) => format!("常量 {} 的声明需带初始化器", val),
      Self::Other(msg) => msg.clone(),
    }
  }
}

impl Error for CompileError {}

impl fmt::Display for CompileError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "\x1b[0;31mCompile Error\x1b[0m {}", self.message())
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
