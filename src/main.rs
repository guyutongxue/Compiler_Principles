use argparse::{Mode, ParsedArgs};
use koopa::back::KoopaGenerator;
use std::env::args;
use std::error::Error;
use std::fs;
use std::io::{stdout, Write};

mod argparse;
mod backend;
mod frontend;
mod optimization;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

fn compile() -> Result<()> {
  let ParsedArgs {
    mode,
    input,
    output,
  } = argparse::parse(args())?;
  
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
      let riscv = backend::generate_riscv(&ir)?;
      output.write(riscv.to_string().as_bytes())?;
    }
    Mode::Perf => {
      let mut riscv = backend::generate_riscv(&ir)?;
      riscv = optimization::pass_peephole(&riscv);
      output.write(riscv.to_string().as_bytes())?;
    }
  }
  Ok(())
}

fn main() {
  if let Err(e) = compile() {
    eprintln!("{}", e);
    std::process::exit(1);
  }
}
