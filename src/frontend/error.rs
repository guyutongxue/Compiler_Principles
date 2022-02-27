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
pub struct CompileError(pub String);

impl Error for CompileError {}

impl fmt::Display for CompileError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "\x1b[0;31mCompile Error\x1b[0m {}", self.0)
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
