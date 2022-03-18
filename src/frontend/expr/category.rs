use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};
use koopa::ir::{TypeKind, Value};

use super::{generate, ToIrValue};
use crate::frontend::error::CompileError;
use crate::{
  frontend::{
    ast::{
      AddExp, AssignExp, EqExp, Exp, LAndExp, LOrExp, MulExp, PostfixExp, PrimaryExp, RelExp,
      UnaryExp,
    },
    decl::GenerateContext,
  },
  Result,
};

#[derive(Debug, Clone, Copy)]
pub enum Category {
  LValue,
  RValue,
}

pub trait GetCategory {
  fn get_category(&self) -> Category;
}

impl GetCategory for Exp {
  fn get_category(&self) -> Category {
    match self {
      Exp::Assign(exp) => exp.get_category(),
      Exp::Comma(_, rhs) => rhs.get_category(),
    }
  }
}

impl GetCategory for AssignExp {
  fn get_category(&self) -> Category {
    match self {
      AssignExp::LOr(exp) => exp.get_category(),
      AssignExp::Assign(..) => Category::LValue,
    }
  }
}

impl GetCategory for LOrExp {
  fn get_category(&self) -> Category {
    match self {
      LOrExp::And(exp) => exp.get_category(),
      LOrExp::Or(..) => Category::RValue,
    }
  }
}

impl GetCategory for LAndExp {
  fn get_category(&self) -> Category {
    match self {
      LAndExp::Eq(exp) => exp.get_category(),
      LAndExp::And(..) => Category::RValue,
    }
  }
}

impl GetCategory for EqExp {
  fn get_category(&self) -> Category {
    match self {
      EqExp::Rel(exp) => exp.get_category(),
      EqExp::Eq(..) => Category::RValue,
    }
  }
}

impl GetCategory for RelExp {
  fn get_category(&self) -> Category {
    match self {
      RelExp::Add(exp) => exp.get_category(),
      RelExp::Rel(..) => Category::RValue,
    }
  }
}

impl GetCategory for AddExp {
  fn get_category(&self) -> Category {
    match self {
      AddExp::Mul(exp) => exp.get_category(),
      AddExp::Add(..) => Category::RValue,
    }
  }
}

impl GetCategory for MulExp {
  fn get_category(&self) -> Category {
    match self {
      MulExp::Unary(exp) => exp.get_category(),
      MulExp::Mul(..) => Category::RValue,
    }
  }
}

impl GetCategory for UnaryExp {
  fn get_category(&self) -> Category {
    match self {
      UnaryExp::Postfix(exp) => exp.get_category(),
      UnaryExp::Deref(..) => Category::LValue,
      UnaryExp::Address(..) => Category::RValue,
      UnaryExp::Op(..) => Category::RValue,
    }
  }
}

impl GetCategory for PostfixExp {
  fn get_category(&self) -> Category {
    match self {
      PostfixExp::Primary(exp) => exp.get_category(),
      PostfixExp::Call(..) => Category::RValue,
      PostfixExp::Subscript(..) => Category::LValue,
    }
  }
}

impl GetCategory for PrimaryExp {
  fn get_category(&self) -> Category {
    match self {
      PrimaryExp::Ident(..) => Category::LValue,
      PrimaryExp::Paren(exp) => exp.get_category(),
      PrimaryExp::Num(..) => Category::RValue,
    }
  }
}

pub trait ExpectCategory<T: ToIrValue> {
  fn expect(&self, category: Category) -> Result<ValueGenerator<T>>;
}

pub struct ValueGenerator<'a, T: ToIrValue>(&'a T, bool);

impl<'a, T: ToIrValue> ValueGenerator<'a, T> {
  pub fn generate(&self, context: &mut GenerateContext) -> Result<Value> {
    let val = generate(self.0, context)?;
    if self.1 {
      match context.value_ty_kind(val) {
        TypeKind::Pointer(base) => match base.kind() {
          TypeKind::Array(..) => {
            let zero = context.dfg().new_value().integer(0);
            let elem = context.dfg().new_value().get_elem_ptr(val, zero);
            context.add_inst(elem)?;
            Ok(elem)
          }
          _ => {
            let load = context.dfg().new_value().load(val);
            context.add_inst(load)?;
            Ok(load)
          }
        },
        _ => {
          // Constexpr value
          Ok(val)
        }
      }
    } else {
      Ok(val)
    }
  }
}

impl<T: ToIrValue> ExpectCategory<T> for T {
  fn expect(&self, category: Category) -> Result<ValueGenerator<T>> {
    match category {
      Category::LValue => match self.get_category() {
        Category::LValue => Ok(ValueGenerator(self, false)),
        Category::RValue => Err(CompileError::NotLValue)?,
      },
      Category::RValue => match self.get_category() {
        Category::RValue => Ok(ValueGenerator(self, false)),
        Category::LValue => Ok(ValueGenerator(self, true)),
      },
    }
  }
}
