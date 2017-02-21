extern crate min_caml_rust;

use min_caml_rust::parser::*;
fn main() {
    println!("Mitou Min-Caml Compiler (C) Eijiro Sumii\n (Port to Rust)");
    println!("{:?}", exp(b" ( c)"));
}
