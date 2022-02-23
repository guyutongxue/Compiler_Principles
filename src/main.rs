use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::error::Error;
use std::fs;

mod ast;
mod ir;
mod riscv;

use riscv::GenerateAsm;

lalrpop_mod!(sysy);

fn main() -> Result<(), Box<dyn Error>> {
  let mut args = args();
  args.next();
  let mode = args.next().expect("missing mode");
  let input = args.next().expect("missing input");
  args.next();
  let output = args.next().expect("missing output");

  let input = fs::read_to_string(input)?;

  let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

  let ir = ir::generate_program(ast)?;

  match mode.as_str() {
    "-koopa" => {
      let output = fs::File::create(output)?;
      KoopaGenerator::new(output).generate_on(&ir)?;
    }
    "-riscv" => {
      fs::write(output, ir.generate()?)?;
    }
    _ => panic!("unknown mode"),
  }

  Ok(())
}
