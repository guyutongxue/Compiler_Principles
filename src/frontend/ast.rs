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
  LOr(LOrExp),
}

#[derive(Debug)]
pub enum LOrExp {
  And(Box<LAndExp>),
  Or(Box<LOrExp>, Box<LAndExp>),
}

#[derive(Debug)]
pub enum LAndExp {
  Eq(Box<EqExp>),
  And(Box<LAndExp>, Box<EqExp>),
}

#[derive(Debug)]
pub enum EqExp {
  Rel(Box<RelExp>),
  Eq(Box<EqExp>, EqOp, Box<RelExp>),
}

#[derive(Debug)]
pub enum EqOp {
  Equal,
  NotEqual,
}

#[derive(Debug)]
pub enum RelExp {
  Add(Box<AddExp>),
  Rel(Box<RelExp>, RelOp, Box<AddExp>),
}

#[derive(Debug)]
pub enum RelOp {
  Less,
  LessEqual,
  Greater,
  GreaterEqual,
}

#[derive(Debug)]
pub enum AddExp {
  Mul(Box<MulExp>),
  Add(Box<AddExp>, AddOp, Box<MulExp>),
}

#[derive(Debug)]
pub enum AddOp {
  Plus,
  Minus,
}

#[derive(Debug)]
pub enum MulExp {
  Unary(Box<UnaryExp>),
  Mul(Box<MulExp>, MulOp, Box<UnaryExp>),
}

#[derive(Debug)]
pub enum MulOp {
  Multiply,
  Divide,
  Modulo,
}

#[derive(Debug)]
pub enum UnaryExp {
  Primary(PrimaryExp),
  Op(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug)]
pub enum UnaryOp {
  Positive,
  Negative,
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
