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

type ParseResult = Result<(), ParseError>;

pub fn recognize(source: &str) -> ParseResult {
  let mut lexer = Token::lexer(source);

  expr(&mut lexer)
}

// Expr -> Term Expr'
fn expr(lexer: &mut Lexer) -> ParseResult {
  println!("Expr starting with {}", lexer.slice());
  term(lexer).and_then({ |_| expr_prime(lexer) })
}

// Expr' -> + Term Expr' | - Term Expr' | E
fn expr_prime(lexer: &mut Lexer) -> ParseResult {
  println!("ExprPrime starting with {}", lexer.slice());
  match lexer.token {
    Token::Add | Token::Subtract => {
      lexer.advance();
      term(lexer).and_then({ |_| expr_prime(lexer) })
    }
    Token::End => Ok(()),
    _ => Err(ParseError::from_lexer(lexer)),
  }
}

// Term -> Factor Term'
fn term(lexer: &mut Lexer) -> ParseResult {
  println!("Term starting with {}", lexer.slice());
  factor(lexer).and_then({ |_| term_prime(lexer) })
}

// Term' -> * Factor Term' | / Factor Term' | E
fn term_prime(lexer: &mut Lexer) -> ParseResult {
  println!("TermPrime starting with {}", lexer.slice());
  match lexer.token {
    Token::Multiply | Token::Divide => {
      lexer.advance();
      factor(lexer).and_then({ |_| term_prime(lexer) })
    }
    _ => Ok(()),
  }
}

// Factor -> Ident | Integer | (Expr)
fn factor(lexer: &mut Lexer) -> ParseResult {
  println!("Factor starting with {}", lexer.slice());
  match lexer.token {
    Token::Ident | Token::Integer => {
      println!("{:?} starting with {}", lexer.token, lexer.slice());
      lexer.advance();
      Ok(())
    }
    Token::LParen => {
      lexer.advance();
      expr(lexer).and_then({
        |_| match lexer.token {
          Token::RParen => {
            lexer.advance();
            Ok(())
          }
          _ => Err(ParseError::from_lexer(lexer)),
        }
      })
    }
    _ => Err(ParseError::from_lexer(lexer)),
  }
}
