extern crate min_caml_rust;
extern crate nom;

use min_caml_rust::{id, parser, k_normal, typing};
use min_caml_rust::syntax::Type;
use nom::IResult;
use std::collections::HashMap;

fn main() {
    println!("Mitou Min-Caml Compiler (C) Eijiro Sumii\n (Port to Rust)");
    let program = b"(let rec f x = () in f 1)";
    let extenv = vec![].into_iter().collect();
    run(program, extenv);
}

fn run(program: &[u8], extenv: HashMap<String, Type>) {
    let mut id_gen = id::IdGen::new();
    let expr = match parser::exp(program) {
        IResult::Done(_, expr) => expr,
        IResult::Incomplete(_) => panic!("incomplete"),
        IResult::Error(alt) => panic!(format!("error: {:?}", alt)),
    };
    let expr = parser::uniqify(expr, &mut id_gen);
    println!("expr = {:?}", expr);
    let (expr, _extenv) = match typing::f(&expr, &mut id_gen) {
        Ok(x) => x,
        Err(msg) => panic!(format!("error typecheck: {}", msg)),
    };
    let k_normal = k_normal::f(expr, &mut id_gen, &extenv);
    println!("{:?}", k_normal);
}

#[cfg(test)]
mod tests {
    use min_caml_rust::syntax::Type;
    use run;
    #[test]
    fn test_bunch_of_functions() {
        let program = br#"
print_int
  (int_of_float
     ((sin (cos (sqrt (abs_float (12.3))))
         +. 4.5 -. 6.7 *. 8.9 /. 1.23456789)
        *. float_of_int 1000000))
"#;;
        let extenv = vec![("sin".to_string(),
                           Type::Fun(vec![Type::Float].into_boxed_slice(),
                                     Box::new(Type::Float))),
                          ("cos".to_string(),
                           Type::Fun(vec![Type::Float].into_boxed_slice(),
                                     Box::new(Type::Float))),
                          ("sqrt".to_string(),
                           Type::Fun(vec![Type::Float].into_boxed_slice(),
                                     Box::new(Type::Float))),
                          ("abs_float".to_string(),
                           Type::Fun(vec![Type::Float].into_boxed_slice(),
                                     Box::new(Type::Float))),
                          ("int_of_float".to_string(),
                           Type::Fun(vec![Type::Float].into_boxed_slice(),
                                     Box::new(Type::Int))),
                          ("float_of_int".to_string(),
                           Type::Fun(vec![Type::Int].into_boxed_slice(),
                                     Box::new(Type::Float))),
                          ("print_int".to_string(),
                           Type::Fun(vec![Type::Int].into_boxed_slice(),
                                     Box::new(Type::Unit))),
        ].into_iter().collect();
        run(program, extenv);
    }
}
