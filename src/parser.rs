use logos::Logos;

#[derive(Logos, Debug, Copy, Clone)]
enum Token {
  #[end]
  End,

  #[error]
  Error,

  #[token = "="]
  Assign,

  #[token = "+"]
  Add,

  #[token = "-"]
  Sub,

  #[token = "*"]
  Mult,

  #[token = "/"]
  Div,

  #[token = "("]
  LParen,

  #[token = ")"]
  RParen,

  #[regex = "[a-zA-Z_][a-zA-Z0-9_]*"]
  Ident,

  #[regex = "[0-9]+"]
  Integer,
}

type Lexer<'a> = logos::Lexer<Token, &'a str>;

#[derive(Debug)]
pub struct ParseError {
  pub msg: String,
  pub token: String,
  pub range: std::ops::Range<usize>,
}

impl ParseError {
  fn from_lexer(msg: String, lexer: &Lexer) -> ParseError {
    ParseError {
      msg,
      token: lexer.slice().to_owned(),
      range: lexer.range(),
    }
  }
}

// Expr -> Term Expr'
#[derive(Debug)]
pub enum Expr {
  Expr(Term, ExprPrime),
}

// Expr' -> + Term Expr' | - Term Expr' | E
#[derive(Debug)]
pub enum ExprPrime {
  Add(Term, Box<ExprPrime>),
  Sub(Term, Box<ExprPrime>),
  None,
}

// Term -> Factor Term'
#[derive(Debug)]
pub enum Term {
  Term(Factor, TermPrime),
}

// Term' -> * Factor Term' | / Factor Term' | E
#[derive(Debug)]
pub enum TermPrime {
  Mult(Factor, Box<TermPrime>),
  Div(Factor, Box<TermPrime>),
  None,
}

// Factor -> Ident | Integer | (Expr)
#[derive(Debug)]
pub enum Factor {
  Ident(String),
  Integer(i32),
  // Expr must be boxed because the recursive definition makes size impossible to compute.
  // Since Box<Expr> is just a pointer, its size is known.
  Expr(Box<Expr>),
}

fn child_prefix(prefix: &str) -> String {
  format!("{}  |", prefix)
}

trait TreePrintable {
  fn print_tree(&self, prefix: String) -> String;
}

impl TreePrintable for Expr {
  fn print_tree(&self, prefix: String) -> String {
    use self::Expr::Expr;

    match self {
      Expr(term, expr_prime) => print_tree(
        &prefix,
        "Expr",
        vec![
          term.print_tree(child_prefix(&prefix)),
          expr_prime.print_tree(child_prefix(&prefix)),
        ],
      ),
    }
  }
}

impl TreePrintable for ExprPrime {
  fn print_tree(&self, prefix: String) -> String {
    use ExprPrime::{Add, None, Sub};

    let vec = match self {
      Add(term, expr_prime) | Sub(term, expr_prime) => vec![
        match self {
          Add(_, _) => "Add".to_owned(),
          Sub(_, _) => "Sub".to_owned(),
          _ => unreachable!(),
        },
        term.print_tree(child_prefix(&prefix)),
        expr_prime.print_tree(child_prefix(&prefix)),
      ],
      None => vec![],
    };

    print_tree(&prefix, "ExprPrime", vec![])
  }
}

impl TreePrintable for Term {
  fn print_tree(&self, prefix: String) -> String {
    match self {
      Term::Term(factor, term_prime) => print_tree(
        &prefix,
        "Term",
        vec![]
        //   factor.print_tree(&child_prefix(prefix)),
        //   term_prime.print_tree(&child_prefix(prefix)),
        // ],
      ),
    }
  }
}

impl TreePrintable for TermPrime {
  fn print_tree(&self, prefix: String) -> String {
    "TermPrime".to_owned()
  }
}

impl TreePrintable for Factor {
  fn print_tree(&self, prefix: String) -> String {
    "Factor".to_owned()
  }
}

fn print_tree(prefix: &String, node: &str, children: Vec<String>) -> String {
  // Sum of lengths of: prefix + node + each child prefix + a newline for each child
  // (child prefix has length 'prefix.len() + 5' due to the added '  |- ')
  let mut str_len = prefix.len() + node.len() + (prefix.len() + 6) * children.len();
  // Add the length of the name of each child to 'str_len'
  for child in &children {
    str_len += child.len();
  }
  let mut tree = String::with_capacity(str_len);
  tree.push_str(&format!("{}- {}", prefix, node));

  if children.len() == 0 {
    return tree;
  }

  for child in children[..children.len() - 1].iter() {
    tree.push_str(&format!("\n{}{}", prefix, child));
  }
  tree.push_str(&format!("\n{}{}", prefix, children[children.len() - 1]));
  tree
}

use std::fmt;

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.print_tree("".to_owned()))
  }
}

pub fn parse(source: &str) -> Result<Expr, ParseError> {
  let mut lexer = Token::lexer(source);

  expr(&mut lexer)
}

// Expr -> Term Expr'
fn expr(lexer: &mut Lexer) -> Result<Expr, ParseError> {
  use self::Expr::Expr;
  let term = term(lexer)?;
  let expr_prime = expr_prime(lexer)?;
  Ok(Expr(term, expr_prime))
}

// Expr' -> + Term Expr' | - Term Expr' | E
fn expr_prime(lexer: &mut Lexer) -> Result<ExprPrime, ParseError> {
  use ExprPrime::{Add, None, Sub};
  match lexer.token {
    op @ Token::Add | op @ Token::Sub => {
      lexer.advance();
      let term = term(lexer)?;
      let expr_prime = expr_prime(lexer)?;

      match op {
        Token::Add => Ok(Add(term, Box::new(expr_prime))),
        Token::Sub => Ok(Sub(term, Box::new(expr_prime))),
        _ => unreachable!(),
      }
    }
    _ => Ok(None),
  }
}

// Term -> Factor Term'
fn term(lexer: &mut Lexer) -> Result<Term, ParseError> {
  use self::Term::Term;

  let factor = factor(lexer)?;
  let term_prime = term_prime(lexer)?;

  Ok(Term(factor, term_prime))
}

// Term' -> * Factor Term' | / Factor Term' | E
fn term_prime(lexer: &mut Lexer) -> Result<TermPrime, ParseError> {
  use TermPrime::{Div, Mult, None};
  match lexer.token {
    op @ Token::Mult | op @ Token::Div => {
      lexer.advance();
      let factor = factor(lexer)?;
      let term_prime = term_prime(lexer)?;

      match op {
        Token::Mult => Ok(Mult(factor, Box::new(term_prime))),
        Token::Div => Ok(Div(factor, Box::new(term_prime))),
        _ => unreachable!(),
      }
    }
    _ => Ok(None),
  }
}

// Factor -> Ident | Integer | (Expr)
fn factor(lexer: &mut Lexer) -> Result<Factor, ParseError> {
  use Factor::{Expr, Ident, Integer};
  match lexer.token {
    Token::Ident => {
      let id = Ident(lexer.slice().to_owned());
      lexer.advance();
      Ok(id)
    }
    Token::Integer => {
      let int = Integer(lexer.slice().parse().unwrap());
      lexer.advance();
      Ok(int)
    }
    Token::LParen => {
      lexer.advance();
      let expr = expr(lexer)?;
      match lexer.token {
        Token::RParen => Ok(Expr(Box::new(expr))),
        _ => Err(ParseError::from_lexer(
          format!("Expected ')' but found '{}'", lexer.slice()),
          lexer,
        )),
      }
    }
    _ => Err(ParseError::from_lexer(
      format!(
        "Expected one of: integer, identifier or '(', found {}",
        lexer.slice()
      ),
      lexer,
    )),
  }
}
