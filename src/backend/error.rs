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
pub struct OutOfRegistersError;

impl Error for OutOfRegistersError {}

impl fmt::Display for OutOfRegistersError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "out of registers")
  }
}
