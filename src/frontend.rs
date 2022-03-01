use koopa::ir::Program;
use lalrpop_util::lalrpop_mod;

use self::{
  ast::{FuncDecl, TypeSpec, Decl, Param},
  error::CompileError,
};

mod ast;
mod ty;
mod consteval;
mod error;
mod expr;
mod ir;
mod stmt;
mod symbol;

lalrpop_mod!(parser, "/frontend/sysy.rs");

macro_rules! generate_prelude {
  ( $( ( $name:expr, [ $( $param:expr ),* ] ) ),* ) => {
    {
    let mut func_decls = vec![];
    $(
      let mut params: Vec<Param> = vec![];
      let mut i = 0;
      $(
        params.push($param(format!("p{}", i)));
        i = i + 1;
      )*

      func_decls.push(Decl::Func(FuncDecl {
        func_type: TypeSpec::Int,
        params: params,
        ident: $name.into(),
        body: None,
      }));
    )*
    func_decls
  }};
}

// https://github.com/rust-lang/rust/issues/24580
#[allow(unused_mut, unused_variables, unused_assignments)]
pub fn generate_ir(input: String) -> Result<Program, Box<dyn std::error::Error>> {
  let mut ast = parser::CompUnitParser::new()
    .parse(&input)
    .map_err(|e| CompileError::Other(e.to_string()))?;

  // 参考 https://github.com/pku-minic/sysy-runtime-lib/blob/master/src/sysy.h
  // 
  // 按标准，SysY 具有类似 C89 “隐式函数声明” 的语义。
  // 但这并不是一个很漂亮的语法。因此在这个实现中，
  // 采用类似 Rust prelude 的语法。以下 SysY 库函数（定义于 sysy.h）
  // 将默认添加到每一个源文件开头。
  let prelude = generate_prelude![
    ("getint", []),
    ("getch", []),
    ("getarray", [Param::Pointer]),
    ("putint", [Param::Int]),
    ("putch", [Param::Int]),
    ("putarray", [Param::Int, Param::Pointer]),
    ("starttime", []),
    ("stoptime", [])
  ];

  ast = prelude.into_iter().chain(ast.into_iter()).collect();

  ir::generate_program(ast)
}
