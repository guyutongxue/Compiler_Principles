use std::{error::Error, fmt};


#[derive(Debug)]
pub struct UnimplementedError(pub Box<dyn fmt::Debug>);

impl Error for UnimplementedError {}

impl fmt::Display for UnimplementedError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:#?} unimplemented", self.0)
  }
}

#[derive(Debug)]
pub struct StackOverflowError;

impl Error for StackOverflowError {}

impl fmt::Display for StackOverflowError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "stack overflow")
  }
}
