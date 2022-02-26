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

lalrpop_mod!(parser, "/frontend/sysy.rs");

pub fn generate_ir(input: String) -> Result<Program, Box<dyn std::error::Error>> {
  let ast = parser::CompUnitParser::new()
    .parse(&input)
    .map_err(|e| CompileError(e.to_string()))?;
  ir::generate_program(ast)
}
