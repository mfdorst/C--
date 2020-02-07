use crate::parser;

use parser::{Expr, ExprPrime, Factor, Term, TermPrime};
use std::fmt::Debug;

pub trait Ast: Debug {}

#[derive(Debug)]
pub enum Op {
  Add(Box<dyn Ast>, Box<dyn Ast>),
  Sub(Box<dyn Ast>, Box<dyn Ast>),
  Mul(Box<dyn Ast>, Box<dyn Ast>),
  Div(Box<dyn Ast>, Box<dyn Ast>),
}

impl Ast for Op {}

#[derive(Debug)]
pub enum Value {
  Ident(String),
  Integer(i32),
}

impl Ast for Value {}

impl Expr {
  pub fn to_ast(&self) -> Box<dyn Ast> {
    match self {
      Expr::Expr(term, expr_pr) => expr_pr.to_ast(term),
    }
  }
}

impl ExprPrime {
  fn to_ast(&self, lterm: &Term) -> Box<dyn Ast> {
    match self {
      ExprPrime::Add(rterm, expr_pr) => Box::new(Op::Add(lterm.to_ast(), expr_pr.to_ast(rterm))),
      ExprPrime::Sub(rterm, expr_pr) => Box::new(Op::Sub(lterm.to_ast(), expr_pr.to_ast(rterm))),
      ExprPrime::None => lterm.to_ast(),
    }
  }
}

impl Term {
  fn to_ast(&self) -> Box<dyn Ast> {
    match self {
      Term::Term(factor, term_pr) => term_pr.to_ast(factor),
    }
  }
}

impl TermPrime {
  fn to_ast(&self, lfactor: &Factor) -> Box<dyn Ast> {
    match self {
      TermPrime::Mult(rfactor, term_pr) => {
        Box::new(Op::Mul(lfactor.to_ast(), term_pr.to_ast(rfactor)))
      }
      TermPrime::Div(rfactor, term_pr) => {
        Box::new(Op::Div(lfactor.to_ast(), term_pr.to_ast(rfactor)))
      }
      TermPrime::None => lfactor.to_ast(),
    }
  }
}

impl Factor {
  fn to_ast(&self) -> Box<dyn Ast> {
    match self {
      Factor::Ident(id) => Box::new(Value::Ident(id.to_owned())),
      Factor::Integer(i) => Box::new(Value::Integer(*i)),
      Factor::Expr(expr) => expr.to_ast(),
    }
  }
}
