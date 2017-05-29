extern crate min_caml_rust;
#[macro_use]
extern crate lazy_static;
extern crate nom;

use min_caml_rust::{id, parser, k_normal, typing};
use min_caml_rust::syntax::Type;
use nom::IResult;
use std::collections::HashMap;
use std::path::Path;
use std::fs::File;
use std::io::Read;

lazy_static! {
    static ref EXTENV: HashMap<String, Type>
        = vec![("sin".to_string(),
                Type::Fun(Box::new([Type::Float]),
                          Box::new(Type::Float))),
               ("cos".to_string(),
                Type::Fun(Box::new([Type::Float]),
                          Box::new(Type::Float))),
               ("sqrt".to_string(),
                Type::Fun(Box::new([Type::Float]),
                          Box::new(Type::Float))),
               ("abs_float".to_string(),
                Type::Fun(Box::new([Type::Float]),
                          Box::new(Type::Float))),
               ("int_of_float".to_string(),
                Type::Fun(Box::new([Type::Float]),
                          Box::new(Type::Int))),
               ("float_of_int".to_string(),
                Type::Fun(Box::new([Type::Int]),
                          Box::new(Type::Float))),
               ("print_int".to_string(),
                Type::Fun(Box::new([Type::Int]),
                          Box::new(Type::Unit))),
        ].into_iter().collect();
}


fn main() {
    println!("Mitou Min-Caml Compiler (C) Eijiro Sumii\n (Port to Rust)");
    let args: Vec<String> = std::env::args().collect();
    if args.len() <= 1 {
        println!(" usage: ./min-caml-rust [ML FILENAME]");
        return;
    }
    let ref filename = args[1];
    let path = Path::new(&filename);
    let program = read_from_file(&path).unwrap();
    run(&program);
}

fn read_from_file(path: &Path) -> Result<Vec<u8>, std::io::Error> {
    let mut file = File::open(path)?;
    let mut program = Vec::new();
    file.read_to_end(&mut program)?;
    Ok(program)
}

fn run(program: &[u8]) {
    let mut id_gen = id::IdGen::new();
    let program = match parser::remove_comments(program) {
        Ok(p) => p,
        Err(msg) => panic!(msg),
    };
    println!("comment-removed: {:?}", String::from_utf8(program.clone()));
    let expr = match parser::exp(&program) {
        IResult::Done(_, expr) => expr,
        IResult::Incomplete(_) => panic!("incomplete"),
        IResult::Error(alt) => panic!(format!("error: {:?}", alt)),
    };
    let expr = parser::uniquify(expr, &mut id_gen);
    println!("expr = {:?}", expr);
    let extenv = EXTENV.clone();
    let (expr, _extenv) = match typing::f(&expr, &mut id_gen) {
        Ok(x) => x,
        Err(msg) => panic!(format!("error typecheck: {}", msg)),
    };
    println!("typed expr = {:?}", expr);
    let k_normal = k_normal::f(expr, &mut id_gen, &extenv);
    println!("{:?}", k_normal);
}

#[cfg(test)]
mod tests {
    use run;
    #[test]
    fn test_bunch_of_functions() {
        let program = br#"
print_int
  (int_of_float
     ((sin (cos (sqrt (abs_float (12.3))))
         +. 4.5 -. 6.7 *. 8.9 /. 1.23456789)
        *. float_of_int 1000000))
"#;
        run(program);
    }
}
