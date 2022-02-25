use koopa::back::KoopaGenerator;
use std::env::args;
use std::error::Error;
use std::fs;

mod frontend;
mod backend;

use backend::GenerateAsm;

fn compile(mode: &str, input: &str, output: &str) -> Result<(), Box<dyn Error>> {
  let input = fs::read_to_string(input)?;

  let ir = frontend::generate(input)?;

  match mode {
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

fn main() -> Result<(), Box<dyn Error>> {
  let mut args = args();
  args.next();
  let mode = args.next().expect("missing mode");
  let input = args.next().expect("missing input");
  args.next();
  let output = args.next().expect("missing output");

  compile(&mode, &input, &output).or_else(|e| {
    eprintln!("{}", e);
    Err(e)
  })?;

  Ok(())
}
