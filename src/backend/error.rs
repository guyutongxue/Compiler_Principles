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
pub struct LabelNotExistError(pub String);

impl Error for LabelNotExistError {}

impl fmt::Display for LabelNotExistError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "cannot find corresponding label for '{}'", self.0)
  }
}
