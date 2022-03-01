use koopa::ir::Program;
use lalrpop_util::lalrpop_mod;

use self::error::CompileError;

mod ast;
mod consteval;
mod error;
mod expr;
mod ir;
mod stmt;
mod symbol;
mod ty;

lalrpop_mod!(parser, "/frontend/sysy.rs");

// https://github.com/rust-lang/rust/issues/24580
#[allow(unused_mut, unused_variables, unused_assignments)]
pub fn generate_ir(input: String) -> Result<Program, Box<dyn std::error::Error>> {
  let mut ast = parser::CompUnitParser::new()
    .parse(&input)
    .map_err(|e| CompileError::Other(e.to_string()))?;

  ir::generate_program(ast)
}
