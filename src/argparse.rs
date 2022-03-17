use std::env::Args;

#[derive(Debug, Clone, Copy)]
pub enum Mode {
  Koopa,
  Riscv,
  Perf,
}

#[derive(Debug)]
pub struct ParsedArgs {
  pub mode: Mode,
  pub input: Vec<String>,
  pub output: Option<String>,
}

pub fn parse(mut args: Args) -> Result<ParsedArgs, Box<dyn std::error::Error>> {
  let _name = args.next().unwrap();

  let mut mode: Option<Mode> = None;
  let mut input: Vec<String> = vec![];
  let mut output: Option<String> = None;

  let mut pending_output = false;
  let mut set_mode = |m: Mode| -> Result<(), Box<dyn std::error::Error>> {
    if let Some(mode) = mode {
      Err(format!("duplicate mode: {:#?} and {:#?}", mode, m).into())
    } else {
      mode = Some(m);
      Ok(())
    }
  };

  for i in args {
    if pending_output {
      output = Some(i);
      pending_output = false;
    } else if i.starts_with("-") {
      match i.as_str() {
        "-koopa" => set_mode(Mode::Koopa)?,
        "-riscv" => set_mode(Mode::Riscv)?,
        "-perf" => set_mode(Mode::Perf)?,
        "-o" => pending_output = true,
        _ => return Err(format!("unknown option: {}", i).into()),
      }
    } else {
      input.push(i);
    }
  }
  let mode = mode.ok_or("missing mode")?;
  if input.len() == 0 {
    return Err("missing input".into());
  }
  Ok(ParsedArgs { mode, input, output })
}
