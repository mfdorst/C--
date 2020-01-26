#![allow(non_snake_case)]

mod parser;

fn main() {
  let result = parser::recognize("abc + def * 123 - 456");

  println!("{:?}", result);
}
