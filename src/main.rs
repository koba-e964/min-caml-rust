extern crate min_caml_rust;

use min_caml_rust::{id, parser, k_normal};
use min_caml_rust::syntax::Type;
fn main() {
    println!("Mitou Min-Caml Compiler (C) Eijiro Sumii\n (Port to Rust)");
    let program = b"rand.(3)";
    let (_rest, expr) = parser::exp(program).unwrap();
    let mut id_gen = id::IdGen::new();
    let extenv = vec![("rand".to_string(),
                       Type::Array(Box::new(Type::Int)))].into_iter().collect();
    let k_normal = k_normal::f(expr, &mut id_gen, &extenv);
    println!("{:?}", k_normal);
}
