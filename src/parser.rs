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
