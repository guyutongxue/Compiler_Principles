use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::error::Error;
use std::fs::read_to_string;

mod ast;

lalrpop_mod!(sysy);

fn main() -> Result<(), Box<dyn Error>> {
  let mut args = args();
  args.next();
  let mode = args.next().expect("missing mode");
  let input = args.next().expect("missing input");
  args.next();
  let output = args.next().expect("missing output");

  let input = read_to_string(input)?;

  let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

  println!("{:#?}", ast);
  Ok(())
}
