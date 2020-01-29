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

impl TreeViewable for Expr {
  fn root(&self) -> String {
    "Expr".to_owned()
  }

  fn get_children(&self) -> Vec<Box<&dyn TreeViewable>> {
    match self {
      Expr::Expr(term, expr_pr) => vec![Box::new(term), Box::new(expr_pr)],
    }
  }
}

// Expr' -> + Term Expr' | - Term Expr' | E
#[derive(Debug)]
pub enum ExprPrime {
  Add(Term, Box<ExprPrime>),
  Sub(Term, Box<ExprPrime>),
  None,
}

impl TreeViewable for ExprPrime {
  fn root(&self) -> String {
    use ExprPrime::{Add, None, Sub};
    match self {
      Add(_, _) => "Expr' (+)",
      Sub(_, _) => "Expr' (+)",
      None => "Expr' (Empty)",
    }
    .to_owned()
  }

  fn get_children(&self) -> Vec<Box<&dyn TreeViewable>> {
    use ExprPrime::{Add, None, Sub};
    match self {
      Add(term, expr_prime) | Sub(term, expr_prime) => {
        vec![Box::new(term), Box::new(&**expr_prime)]
      }
      None => vec![],
    }
  }
}

// Term -> Factor Term'
#[derive(Debug)]
pub enum Term {
  Term(Factor, TermPrime),
}

impl TreeViewable for Term {
  fn root(&self) -> String {
    "Term".to_owned()
  }

  fn get_children(&self) -> Vec<Box<&dyn TreeViewable>> {
    let Term::Term(factor, term_prime) = self;

    vec![Box::new(factor), Box::new(term_prime)]
  }
}

// Term' -> * Factor Term' | / Factor Term' | E
#[derive(Debug)]
pub enum TermPrime {
  Mult(Factor, Box<TermPrime>),
  Div(Factor, Box<TermPrime>),
  None,
}

impl TreeViewable for TermPrime {
  fn root(&self) -> String {
    use TermPrime::{Div, Mult, None};
    match self {
      Mult(_, _) => "Term' (*)",
      Div(_, _) => "Term' (/)",
      None => "Term' (Empty)",
    }
    .to_owned()
  }

  fn get_children(&self) -> Vec<Box<&dyn TreeViewable>> {
    use TermPrime::{Div, Mult, None};
    match self {
      Mult(factor, term_prime) | Div(factor, term_prime) => {
        vec![Box::new(factor), Box::new(&**term_prime)]
      }
      None => vec![],
    }
  }
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

impl TreeViewable for Factor {
  fn root(&self) -> String {
    use Factor::{Expr, Ident, Integer};
    match self {
      Ident(id) => format!(r#"Factor ("{}")"#, id),
      Integer(int) => format!("Factor ({})", int),
      Expr(_) => "Factor (Expr)".to_owned(),
    }
  }

  fn get_children(&self) -> Vec<Box<&dyn TreeViewable>> {
    use Factor::Expr;
    match self {
      Expr(expr) => vec![Box::new(&**expr)],
      _ => vec![],
    }
  }
}

trait TreeViewable {
  fn get_children(&self) -> Vec<Box<&dyn TreeViewable>>;
  fn root(&self) -> String;
}

fn tree_view_impl(
  tree: &dyn TreeViewable,
  current_prefix: String,
  next_prefix: String,
  last: bool,
) -> String {
  let mut view = if last {
    format!("{}└- {}\n", current_prefix, tree.root())
  } else {
    format!("{}├- {}\n", current_prefix, tree.root())
  };

  let children = tree.get_children();
  if children.len() == 0 {
    return view;
  }
  let last_index = children.len() - 1;
  for (i, child) in children.iter().enumerate() {
    if i < last_index {
      view.push_str(&tree_view_impl(
        **child,
        next_prefix.clone(),
        format!("{}|  ", next_prefix.clone()),
        false,
      ))
    } else {
      view.push_str(&tree_view_impl(
        **child,
        next_prefix.clone(),
        format!("{}   ", next_prefix.clone()),
        true,
      ))
    }
  }

  view
}

fn tree_view(tree: &dyn TreeViewable) -> String {
  tree_view_impl(tree, "".to_owned(), "   ".to_owned(), true)
}

impl std::fmt::Display for Expr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", tree_view(self))
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
