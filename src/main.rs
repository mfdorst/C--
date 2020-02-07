#![allow(non_snake_case)]

mod ast;
mod parser;

fn main() {
  let expr = "abc * (def + 3) * 45";
  let result = parser::parse(expr);

  match result {
    Ok(res) => println!("{}\n\n{}\n\n{}", expr, res, res.to_ast()),
    Err(e) => println!("{}", e.msg),
  }
}
