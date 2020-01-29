#![allow(non_snake_case)]

mod parser;

fn main() {
  let expr = "abc * (def + 3) * 45";
  let result = parser::parse(expr);

  match result {
    Ok(res) => println!("{}\n\n{}", expr, res),
    Err(e) => println!("{}", e.msg),
  }
}
