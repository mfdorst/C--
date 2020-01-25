#![allow(non_snake_case)]

use logos::Logos;

#[derive(Logos, Debug)]
enum Token {
  #[end]
  End,

  #[error]
  Error,

  #[token = "let"]
  Let,

  #[token = "="]
  Assign,

  #[regex = "[a-zA-Z_][a-zA-Z0-9_]*"]
  Ident,

  #[regex = "[0-9]+"]
  Integer,

  #[token = "\n"]
  Newline,
}

fn main() {
  let mut lexer = Token::lexer("let x = 5\nlet y = 3");

  println!("{:?}", lexer.token);
  println!("{:?}", lexer.slice());
  println!("{:?}", lexer.range());
  lexer.advance();
  println!("{:?}", lexer.token);
  println!("{:?}", lexer.slice());
  println!("{:?}", lexer.range());
  lexer.advance();
  println!("{:?}", lexer.token);
  println!("{:?}", lexer.slice());
  println!("{:?}", lexer.range());
  lexer.advance();
  println!("{:?}", lexer.token);
  println!("{:?}", lexer.slice());
  println!("{:?}", lexer.range());
  lexer.advance();
  println!("{:?}", lexer.token);
  println!("{:?}", lexer.slice());
  println!("{:?}", lexer.range());
  lexer.advance();
  println!("{:?}", lexer.token);
  println!("{:?}", lexer.slice());
  println!("{:?}", lexer.range());
  lexer.advance();
  println!("{:?}", lexer.token);
  println!("{:?}", lexer.slice());
  println!("{:?}", lexer.range());
  lexer.advance();
  println!("{:?}", lexer.token);
  println!("{:?}", lexer.slice());
  println!("{:?}", lexer.range());
}
