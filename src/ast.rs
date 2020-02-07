use crate::parser;

use parser::{Expr, ExprPrime, Factor, Term, TermPrime};
use std::fmt::Debug;

#[derive(Debug)]
pub enum Ast {
  Add(Box<Ast>, Box<Ast>),
  Sub(Box<Ast>, Box<Ast>),
  Mul(Box<Ast>, Box<Ast>),
  Div(Box<Ast>, Box<Ast>),
  Ident(String),
  Integer(i32),
}

impl Expr {
  pub fn to_ast(&self) -> Box<Ast> {
    match self {
      Expr::Expr(term, expr_pr) => expr_pr.to_ast(term),
    }
  }
}

impl ExprPrime {
  fn to_ast(&self, lterm: &Term) -> Box<Ast> {
    match self {
      ExprPrime::Add(rterm, expr_pr) => Box::new(Ast::Add(lterm.to_ast(), expr_pr.to_ast(rterm))),
      ExprPrime::Sub(rterm, expr_pr) => Box::new(Ast::Sub(lterm.to_ast(), expr_pr.to_ast(rterm))),
      ExprPrime::None => lterm.to_ast(),
    }
  }
}

impl Term {
  fn to_ast(&self) -> Box<Ast> {
    match self {
      Term::Term(factor, term_pr) => term_pr.to_ast(factor),
    }
  }
}

impl TermPrime {
  fn to_ast(&self, lfactor: &Factor) -> Box<Ast> {
    match self {
      TermPrime::Mult(rfactor, term_pr) => {
        Box::new(Ast::Mul(lfactor.to_ast(), term_pr.to_ast(rfactor)))
      }
      TermPrime::Div(rfactor, term_pr) => {
        Box::new(Ast::Div(lfactor.to_ast(), term_pr.to_ast(rfactor)))
      }
      TermPrime::None => lfactor.to_ast(),
    }
  }
}

impl Factor {
  fn to_ast(&self) -> Box<Ast> {
    match self {
      Factor::Ident(id) => Box::new(Ast::Ident(id.to_owned())),
      Factor::Integer(i) => Box::new(Ast::Integer(*i)),
      Factor::Expr(expr) => expr.to_ast(),
    }
  }
}
