use argparse::{ParsedArgs, Mode};
use koopa::back::KoopaGenerator;
use std::env::args;
use std::error::Error;
use std::fs;
use std::io::{stdout, Write};

mod frontend;
mod backend;
mod argparse;

fn compile(args: ParsedArgs) -> Result<(), Box<dyn Error>> {
  let ParsedArgs { mode, input, output } = args;
  let input = fs::read_to_string(&input[0])?;
  let mut output: Box<dyn Write> = if output.is_none() {
    Box::new(stdout())
  } else {
    Box::new(fs::File::create(output.unwrap())?)
  };

  let ir = frontend::generate_ir(input)?;

  match mode {
    Mode::Koopa => {
      KoopaGenerator::new(output).generate_on(&ir)?;
    }
    Mode::Riscv => {
      let insts = backend::generate_riscv(&ir)?;
      output.write(insts.join("\n").as_bytes())?;
    }
  }
  Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
  let args = argparse::parse(args())?;

  compile(args).or_else(|e| {
    eprintln!("{}", e);
    Err(e)
  })?;

  Ok(())
}
