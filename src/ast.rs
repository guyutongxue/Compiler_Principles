#[derive(Debug)]
pub struct CompUnit {
  pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
  pub func_type: FuncType,
  pub ident: String,
  pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
  Int,
}

#[derive(Debug)]
pub struct Block {
  pub stmt: Stmt,
}

#[derive(Debug)]
pub enum Exp {
  Unary(UnaryExp),
}

#[derive(Debug)]
pub enum UnaryExp {
  Primary(PrimaryExp),
  Op(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug)]
pub enum UnaryOp {
  Pos,
  Neg,
  Not,
}

#[derive(Debug)]
pub enum PrimaryExp {
  Num(i32),
  Paren(Box<Exp>),
}

#[derive(Debug)]
pub struct Stmt {
  pub exp: Exp,
}
