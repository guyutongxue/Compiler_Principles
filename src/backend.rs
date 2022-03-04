mod error;
mod from_func;
mod from_value;
mod riscv;

use std::collections::{HashMap, VecDeque};
use std::io::BufWriter;
use std::sync::RwLock;

use koopa::back::KoopaGenerator;
use koopa::ir::{Function, Program, Type, TypeKind, Value, ValueKind};
use once_cell::sync::Lazy;

use self::error::LabelNotExistError;
use self::riscv::Riscv;
use self::riscv::directive::Directive;
use crate::Result;

static FUNC_NAMES: Lazy<RwLock<HashMap<Function, String>>> = Lazy::new(|| RwLock::default());
static VAR_NAMES: Lazy<RwLock<HashMap<Value, String>>> = Lazy::new(|| RwLock::default());

static DEBUG_INFO: Lazy<RwLock<VecDeque<String>>> = Lazy::new(|| RwLock::default());

fn flatten_initializer(ir: &Program, value: Value) -> Vec<i32> {
  let mut insts = vec![];
  match ir.borrow_value(value).kind() {
    ValueKind::Integer(i) => {
      let i = i.value();
      insts.push(i);
    }
    ValueKind::Aggregate(agg) => {
      for i in agg.elems() {
        insts.extend(flatten_initializer(ir, *i));
      }
    }
    _ => panic!("initializer not integer nor aggregate"),
  }
  insts
}

pub fn generate_riscv(ir: &Program) -> Result<Riscv> {
  Type::set_ptr_size(4);
  // Prepare debug info
  {
    let buf = BufWriter::new(Vec::new());
    let mut gen = KoopaGenerator::new(buf);
    gen.generate_on(ir)?;
    let bytes = gen.writer().into_inner()?;
    let string = String::from_utf8(bytes)?;
    for i in string.split("\n") {
      DEBUG_INFO.write()?.push_back(i.into());
    }
  }

  let mut result = Riscv::new();
  let mut has_global_alloc = false;

  for (&v, vd) in ir.borrow_values().iter() {
    if let ValueKind::GlobalAlloc(alloc) = vd.kind() {
      has_global_alloc = true;
      result.add_comment(DEBUG_INFO.write()?.pop_front().unwrap());
      let name = vd
        .name()
        .clone()
        .ok_or(LabelNotExistError("alloc ???".into()))?;
      let name = name[1..].to_string();
      result.add_directive(Directive::Data);
      result.add_directive(Directive::Globl(name.clone()));
      result.add_label(name.clone());
      let init = alloc.init();
      if matches!(ir.borrow_value(init).kind(), ValueKind::ZeroInit(_)) {
        if let TypeKind::Pointer(base) = vd.ty().kind() {
          result.add_directive(Directive::Zero(base.size() as i32));
        } else {
          panic!("global alloc do not have pointer type");
        }
      } else {
        result.add_directive(Directive::Word(flatten_initializer(ir, init)));
      }
      result.add_empty();

      VAR_NAMES.write()?.insert(v, name);
    }
  }
  if has_global_alloc {
    DEBUG_INFO.write()?.pop_front().unwrap();
  }

  for (&f, fd) in ir.funcs() {
    let func_name = &fd.name()[1..];
    FUNC_NAMES.write()?.insert(f, func_name.into());
  }

  for &func in ir.func_layout() {
    let asm = from_func::generate(ir, func)?;
    result.extend(asm);
  }
  
  Ok(result)
}
