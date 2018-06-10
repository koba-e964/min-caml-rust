extern crate min_caml_rust;
#[macro_use]
extern crate lazy_static;
extern crate nom;

use min_caml_rust::{id, parser, k_normal, typing,
                    alpha, beta, assoc, const_fold,
                    elim, inline, closure, x86};
use min_caml_rust::syntax::Type;
use nom::IResult;
use std::collections::HashMap;
use std::path::Path;
use std::fs::File;
use std::io::Read;

const ITER_MAX: usize = 1000; // The max number of iteration


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
               ("truncate".to_string(),
                Type::Fun(Box::new([Type::Float]),
                          Box::new(Type::Int))),
               ("int_of_float".to_string(),
                Type::Fun(Box::new([Type::Float]),
                          Box::new(Type::Int))),
               ("float_of_int".to_string(),
                Type::Fun(Box::new([Type::Int]),
                          Box::new(Type::Float))),
               ("print_int".to_string(),
                Type::Fun(Box::new([Type::Int]),
                          Box::new(Type::Unit))),
               ("print_newline".to_string(),
                Type::Fun(Box::new([Type::Unit]),
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
    let filename = &args[1];
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
    let mut extenv = EXTENV.clone();
    let expr = match typing::f(&expr, &mut id_gen, &mut extenv) {
        Ok(x) => x,
        Err(msg) => panic!(format!("error typecheck: {}", msg)),
    };
    println!("typed expr = {:?}", expr);
    let k_normal = k_normal::f(expr, &mut id_gen, &extenv);
    println!("k_normal = {}", k_normal.0);
    let alpha = alpha::f(k_normal.0, &mut id_gen);
    println!("alpha = {}", alpha);
    let mut e = alpha;
    for i in 0 .. ITER_MAX {
        let new_e = elim::f(const_fold::f(assoc::f(beta::f(e.clone())), &mut id_gen));
        let new_e = inline::f(new_e, &mut id_gen, 50);
        if e == new_e {
            break;
        }
        println!("iter[{}] = {}", i, new_e);
        e = new_e;
    }
    let closure = closure::f(e);
    println!("closure-trans = {}", closure);
    let virtual_asm = x86::virtual_asm::f(closure, &mut id_gen);
    println!("virtual_asm = {}", virtual_asm);
}
