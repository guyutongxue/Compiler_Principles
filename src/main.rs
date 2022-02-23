use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::error::Error;
use std::fs;

mod ast;

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

  let ir = format!(r"
fun @{}(): i32 {{
%entry:
  ret {}
}}
", ast.func_def.ident, ast.func_def.block.stmt.num);

  fs::write(output, ir)?;

  Ok(())
}
