use std::rc::Rc;

pub type CompUnit = Vec<Decl>;

#[derive(Debug)]
pub struct FuncDecl {
  pub func_type: TypeSpec,
  pub ident: String,
  pub params: ParamList,
  pub body: Option<Block>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeSpec {
  Void,
  Int,
}

pub type ParamList = Vec<Box<Declarator>>;

pub type Block = Vec<BlockItem>;

#[derive(Debug)]
pub enum BlockItem {
  Decl(Decl),
  Stmt(Stmt),
}

#[derive(Debug)]
pub enum Stmt {
  Exp(Option<Box<Exp>>),
  Block(Box<Block>),
  If(Box<Exp>, Box<Stmt>, Option<Box<Stmt>>),
  While(Box<Exp>, Box<Stmt>),
  Break,
  Continue,
  Return(Option<Box<Exp>>),
}

#[derive(Debug)]
pub enum Exp {
  Assign(Box<AssignExp>),
  Comma(Box<Exp>, Box<AssignExp>),
}

#[derive(Debug)]
pub enum AssignExp {
  LOr(Box<LOrExp>),
  Assign(Box<LOrExp>, Box<AssignExp>),
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
  Postfix(Box<PostfixExp>),
  Address(Box<UnaryExp>),
  Deref(Box<UnaryExp>),
  Op(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug)]
pub enum UnaryOp {
  Positive,
  Negative,
  Not,
}

#[derive(Debug)]
pub enum PostfixExp {
  Primary(PrimaryExp),
  Subscript(Box<PostfixExp>, Box<Exp>),
  Call(String, Vec<Box<AssignExp>>),
}

#[derive(Debug)]
pub enum PrimaryExp {
  Num(i32),
  Ident(String),
  Paren(Box<Exp>),
}

#[derive(Debug)]
pub enum Decl {
  Var(DeclaratorAndInitializerList),
  Func(FuncDecl),
}

#[derive(Debug)]
pub struct DeclaratorAndInitializerList {
  pub is_const: bool,
  pub ty: TypeSpec,
  pub list: Vec<DeclaratorAndInitializer>,
}

pub type DeclaratorAndInitializer = (Box<Declarator>, Option<Initializer>);

#[derive(Debug)]
pub enum Declarator {
  Ident(String),
  Pointer(Box<Declarator>),
  Array(Box<Declarator>, Box<Exp>),
}

pub type Initializer = InitializerLike<Box<AssignExp>>;

#[derive(Debug)]
pub enum InitializerLike<T> {
  Simple(T),
  Aggregate(Vec<Rc<InitializerLike<T>>>),
}
