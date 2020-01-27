#![allow(non_snake_case)]

mod parser;

fn main() {
  let result = parser::recognize("abc + def * (3 + xyz)");

  match result {
    Ok(res) => println!("{:?}", res),
    Err(e) => println!("{:?}", e),
  }
}
