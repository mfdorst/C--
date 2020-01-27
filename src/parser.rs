use logos::Logos;

#[derive(Logos, Debug)]
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
  Subtract,

  #[token = "*"]
  Multiply,

  #[token = "/"]
  Divide,

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
  token: String,
  range: std::ops::Range<usize>,
}

impl ParseError {
  fn from_lexer(lexer: &Lexer) -> ParseError {
    ParseError {
      token: lexer.slice().to_owned(),
      range: lexer.range(),
    }
  }
}

#[derive(Debug)]
pub enum Expr {
  Term(Term),
  BinOp(Term, Op, Term),
}

#[derive(Debug)]
pub enum Term {
  Factor(Factor),
  BinOp(Factor, Op, Factor),
}

#[derive(Debug)]
pub enum Op {
  Add,
  Sub,
  Mult,
  Div,
}

#[derive(Debug)]
pub enum Factor {
  Ident(String),
  Integer(i32),
  // Expr must be boxed because the recursive definition makes size impossible to compute.
  // Since Box<Expr> is just a pointer, its size is known.
  Expr(Box<Expr>),
}

pub fn recognize(source: &str) -> Result<Expr, ParseError> {
  let mut lexer = Token::lexer(source);

  expr(&mut lexer)
}

// Expr -> Term Expr'
fn expr(lexer: &mut Lexer) -> Result<Expr, ParseError> {
  match term(lexer) {
    Ok(lterm) => match expr_prime(lexer) {
      Ok(Some((op, rterm))) => Ok(Expr::BinOp(lterm, op, rterm)),
      Ok(None) => Ok(Expr::Term(lterm)),
      Err(e) => Err(e),
    },
    Err(e) => Err(e),
  }
}

// Expr' -> + Term Expr' | - Term Expr' | E
fn expr_prime(lexer: &mut Lexer) -> Result<Option<(Op, Term)>, ParseError> {
  let op = match lexer.token {
    Token::Add => Op::Add,
    Token::Subtract => Op::Sub,
    _ => return Ok(None),
  };
  lexer.advance();
  match term(lexer) {
    Ok(lterm) => match expr_prime(lexer) {
      Ok(Some((_op, _rterm))) => panic!("Not yet implemented"),
      Ok(None) => Ok(Some((op, lterm))),
      Err(e) => Err(e),
    },
    Err(e) => Err(e),
  }
}

// Term -> Factor Term'
fn term(lexer: &mut Lexer) -> Result<Term, ParseError> {
  match factor(lexer) {
    Ok(lfactor) => match term_prime(lexer) {
      Ok(Some((op, rfactor))) => Ok(Term::BinOp(lfactor, op, rfactor)),
      Ok(None) => Ok(Term::Factor(lfactor)),
      Err(e) => Err(e),
    },
    Err(e) => Err(e),
  }
}

// Term' -> * Factor Term' | / Factor Term' | E
fn term_prime(lexer: &mut Lexer) -> Result<Option<(Op, Factor)>, ParseError> {
  let op = match lexer.token {
    Token::Multiply => Op::Mult,
    Token::Divide => Op::Div,
    _ => return Ok(None),
  };
  lexer.advance();
  match factor(lexer) {
    Ok(lfactor) => match term_prime(lexer) {
      Ok(Some((_op, _rfactor))) => panic!("Not yet implemented"),
      Ok(None) => Ok(Some((op, lfactor))),
      Err(e) => Err(e),
    },
    Err(e) => Err(e),
  }
}

// Factor -> Ident | Integer | (Expr)
fn factor(lexer: &mut Lexer) -> Result<Factor, ParseError> {
  match lexer.token {
    Token::Ident => {
      let id = Factor::Ident(lexer.slice().to_owned());
      lexer.advance();
      Ok(id)
    }
    Token::Integer => {
      let int = Factor::Integer(lexer.slice().parse().unwrap());
      lexer.advance();
      Ok(int)
    }
    Token::LParen => {
      lexer.advance();
      expr(lexer).and_then({
        |expr| match lexer.token {
          Token::RParen => {
            lexer.advance();
            Ok(Factor::Expr(Box::new(expr)))
          }
          _ => Err(ParseError::from_lexer(lexer)),
        }
      })
    }
    _ => Err(ParseError::from_lexer(lexer)),
  }
}
