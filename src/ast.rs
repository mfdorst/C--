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

impl Ast {
  fn top_level(&self) -> String {
    match self {
      Ast::Add(_, _) => "+".to_string(),
      Ast::Sub(_, _) => "-".to_string(),
      Ast::Mul(_, _) => "*".to_string(),
      Ast::Div(_, _) => "/".to_string(),
      Ast::Ident(id) => id.clone(),
      Ast::Integer(i) => i.to_string(),
    }
  }

  fn children(&self) -> Vec<&Ast> {
    use Ast::{Add, Div, Mul, Sub};
    match self {
      Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) => vec![&**a, &**b],
      _ => vec![],
    }
  }

  fn to_string(&self) -> String {
    fn tree_view(ast: &Ast, current_prefix: String, next_prefix: String, last: bool) -> String {
      let mut view = if last {
        format!("{}└- {}\n", current_prefix, ast.top_level())
      } else {
        format!("{}├- {}\n", current_prefix, ast.top_level())
      };
      let children = ast.children();
      if children.len() == 0 {
        return view;
      }
      let last_index = children.len() - 1;
      for (i, child) in children.iter().enumerate() {
        if i < last_index {
          view.push_str(&tree_view(
            &child,
            next_prefix.clone(),
            format!("{}|  ", next_prefix.clone()),
            true,
          ))
        } else {
          view.push_str(&tree_view(
            &child,
            next_prefix.clone(),
            format!("{}   ", next_prefix.clone()),
            true,
          ))
        }
      }
      view
    }
    tree_view(self, "".to_owned(), "   ".to_owned(), true)
  }
}

impl std::fmt::Display for Ast {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.to_string())
  }
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
