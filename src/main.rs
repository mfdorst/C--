#![allow(non_snake_case)]

mod parser;

fn main() {
  let result = parser::parse("abc * (def + 3) * 45");

  match result {
    Ok(res) => println!("{:?}", res),
    Err(e) => println!("{}", e.msg),
  }
}
