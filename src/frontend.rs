use koopa::ir::Program;
use lalrpop_util::lalrpop_mod;

mod ast;
mod ir;
mod error;

lalrpop_mod!(parser, "/frontend/sysy.rs");

pub fn generate(input: String) -> Result<Program, Box<dyn std::error::Error>> {
  let ast = parser::CompUnitParser::new().parse(&input).unwrap();
  ir::generate_program(ast)
}
