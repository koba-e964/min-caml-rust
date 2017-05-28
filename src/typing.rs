use syntax::{Syntax, Fundef, Type};
use id::IdGen;
use std::collections::HashMap;

/**
 * typing.rs (typing.ml in original min-caml)
 * Handles typing of Syntax.
 * Change of design: the signature of deref_typ changed (additional tyenv).
 * This is because of our type inference algorithm.
 * Instead of storing decided types in the refcell of Type.Var itself,
 * we store them in tyenv (:HashMap<usize, Type>).
 */

/* Type that represents an exception */
#[derive(Debug)]
enum TypingError {
    Unify(Type, Type),
}

fn deref_typ(e: &Type, tyenv: &HashMap<usize, Type>) -> Type {
    macro_rules! deref_typ_list {
        ($ls:expr) => ($ls.iter().map(|x| deref_typ(x, tyenv))
                       .collect::<Vec<_>>()
                       .into_boxed_slice());
    }
    match *e {
        Type::Fun(ref t1s, ref t2) =>
            Type::Fun(deref_typ_list!(t1s), Box::new(deref_typ(t2, tyenv))),
        Type::Tuple(ref ts) => Type::Tuple(deref_typ_list!(ts)),
        Type::Array(ref t) => Type::Array(Box::new(deref_typ(t, tyenv))),
        Type::Var(ref n) => {
            if let Some(t) = tyenv.get(n) {
                deref_typ(t, tyenv)
            } else {
                println!("uninstantiated type variable {} detected; assuming int@.", n);
                Type::Int
            }
        }
        _ => e.clone(),
    }
}

fn deref_term(e: &Syntax, tyenv: &HashMap<usize, Type>) -> Syntax {
    macro_rules! invoke {
        ($e:expr) => (deref_term($e, tyenv));
    }
    /*
     * Recursively apply deref_term to inside constructors of form C(e1, e2, ...).
     * ($constr, $($e)*)
     */
    macro_rules! pack {
        ($constr:expr, $($e:expr),*) => ($constr($(
            Box::new(invoke!($e))
        ),*));
    }
    /*
     * Recursively apply deref_term to inside constructors of form C(op, e1, e2).
     * ($constr, $($e)*)
     */
    macro_rules! pack_bin {
        ($constr:expr, $op:expr, $e1:expr, $e2:expr) =>
            ({ $constr(*$op, Box::new(invoke!($e1)), Box::new(invoke!($e2))) })
    }
    macro_rules! boxed_array {
        ($ls:expr) => ($ls.iter().map(|x| invoke!(x)).collect::<Vec<_>>()
                       .into_boxed_slice());
    }
    match *e {
        Syntax::Not(ref e) => pack!(Syntax::Not, e),
        Syntax::Neg(ref e) => pack!(Syntax::Neg, e),
        Syntax::IntBin(ref op, ref e1, ref e2) =>
            pack_bin!(Syntax::IntBin, op, e1, e2),
        Syntax::FNeg(ref e) => pack!(Syntax::FNeg, e),
        Syntax::FloatBin(ref op, ref e1, ref e2) =>
            pack_bin!(Syntax::FloatBin, op, e1, e2),
        Syntax::CompBin(ref op, ref e1, ref e2) =>
            pack_bin!(Syntax::CompBin, op, e1, e2),
        Syntax::If(ref e1, ref e2, ref e3) => pack!(Syntax::If, e1, e2, e3),
        Syntax::Let((ref x, ref t), ref e1, ref e2) =>
            Syntax::Let((x.clone(), deref_typ(t, tyenv)), Box::new(invoke!(e1)),
                        Box::new(invoke!(e2))),
        Syntax::LetRec(ref fundef, ref e2) => {
            let (ref x, ref t) = fundef.name;
            let yts = &fundef.args;
            let e1 = &fundef.body;
            Syntax::LetRec(Fundef {
                name: (x.to_string(), deref_typ(t, tyenv)),
                args: yts.iter()
                    .map(|&(ref x, ref t)| (x.clone(), deref_typ(t, tyenv)))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                body: Box::new(invoke!(e1))
            }, Box::new(invoke!(e2)))
        },
        Syntax::App(ref e, ref es) =>
            Syntax::App(Box::new(invoke!(e)), boxed_array!(es)),
        Syntax::Tuple(ref es) => Syntax::Tuple(boxed_array!(es)),
        Syntax::LetTuple(ref xts, ref e1, ref e2) => {
            let xts: Box<_> = xts.iter().map(|&(ref x, ref t)| (x.clone(), deref_typ(t, tyenv))).collect::<Vec<_>>().into_boxed_slice();
            Syntax::LetTuple(xts, Box::new(invoke!(e1)), Box::new(invoke!(e2)))
        },
        Syntax::Array(ref e1, ref e2) => pack!(Syntax::Array, e1, e2),
        Syntax::Get(ref e1, ref e2) => pack!(Syntax::Get, e1, e2),
        Syntax::Put(ref e1, ref e2, ref e3) => pack!(Syntax::Put, e1, e2, e3),
        _ => e.clone(),
    }
}

/*
 * Checks if type variable r1 occurs in ty.
 */
fn occur(r1: usize, ty: &Type) -> bool {
    macro_rules! occur_list {
        ($ls:expr) => ($ls.iter().any(|ty| occur(r1, ty)))
    }
    match *ty {
        Type::Fun(ref t2s, ref t2) => occur_list!(t2s) || occur(r1, t2),
        Type::Tuple(ref t2s) => occur_list!(t2s),
        Type::Array(ref t2) => occur(r1, t2),
        Type::Var(r2) => r1 == r2,
        _ => false,
    }
}

fn unify(t1: &Type, t2: &Type,
         extenv: &mut HashMap<String, Type>,
         tyenv: &mut HashMap<usize, Type>) -> Result<(), TypingError> {
    println!("unify {:?} {:?}", t1, t2);
    macro_rules! invoke {
        ($t1:expr, $t2:expr) => (unify($t1, $t2, extenv, tyenv));
    }
    macro_rules! unify_seq {
        ($t1s:expr, $t2s: expr) => {
            let n = $t1s.len();
            let m = $t2s.len();
            if n != m {
                return Err(TypingError::Unify(t1.clone(), t2.clone()));
            }
            for i in 0 .. n {
                try!(invoke!(&$t1s[i], &$t2s[i]));
            }
        }
    }
    match (t1.clone(), t2.clone()) { // TODO ugly hack
        (Type::Unit, Type::Unit) => Ok(()),
        (Type::Bool, Type::Bool) => Ok(()),
        (Type::Int, Type::Int) => Ok(()),
        (Type::Float, Type::Float) => Ok(()),
        (Type::Fun(ref t1s, ref t1cod), Type::Fun(ref t2s, ref t2cod)) => {
            unify_seq!(t1s, t2s);
            invoke!(t1cod, t2cod)
        },
        (Type::Tuple(ref t1s), Type::Tuple(ref t2s)) => {
            unify_seq!(t1s, t2s);
            Ok(())
        },
        (Type::Array(ref t1), Type::Array(ref t2)) => invoke!(t1, t2),
        (Type::Var(ref n1), Type::Var(ref n2)) if n1 == n2 => Ok(()),
        (Type::Var(ref n1), _) => {
            if let Some(t1sub) = tyenv.get(n1).cloned() {
                invoke!(&t1sub, t2)
            } else {
                if occur(*n1, t2) {
                    return Err(TypingError::Unify(t1.clone(), t2.clone()));
                }
                tyenv.insert(*n1, t2.clone());
                Ok(())
            }
        },
        (_, Type::Var(_)) => invoke!(t2, t1),
        _ => Err(TypingError::Unify(t1.clone(), t2.clone())),
    }
}


fn g(env: &HashMap<String, Type>, e: &Syntax,
     extenv: &mut HashMap<String, Type>,
     tyenv: &mut HashMap<usize, Type>,
     id_gen: &mut IdGen) -> Result<Type, TypingError> {
    macro_rules! invoke {
        ($env:expr, $e:expr) => (g($env, $e, extenv, tyenv, id_gen));
    }
    macro_rules! typed {
        ($e:expr, $ty:expr) => (try!(unify(&$ty, &try!(invoke!(env, $e)), extenv, tyenv)));
    }
    /* map (g env) to an &[Syntax]
     * &[Syntax] -> Box<[Type]>
     */
    macro_rules! g_seq {
        ($es:expr) => ({
            let mut argtype = Vec::new();
            for e in $es.iter() {
                argtype.push(try!(invoke!(env, e)));
            }
            argtype.into_boxed_slice()
        });
    }
    match *e {
        Syntax::Unit => Ok(Type::Unit),
        Syntax::Bool(_) => Ok(Type::Bool),
        Syntax::Int(_) => Ok(Type::Int),
        Syntax::Float(_) => Ok(Type::Float),
        Syntax::Not(ref e) => {
            typed!(e, Type::Bool);
            Ok(Type::Bool)
        },
        Syntax::Neg(ref e) => {
            typed!(e, Type::Int);
            Ok(Type::Int)
        },
        Syntax::IntBin(_, ref e1, ref e2) => {
            typed!(e1, Type::Int);
            typed!(e2, Type::Int);
            Ok(Type::Int)
        },
        Syntax::FNeg(ref e) => {
            typed!(e, Type::Float);
            Ok(Type::Float)
        },
        Syntax::FloatBin(_, ref e1, ref e2) => {
            typed!(e1, Type::Float);
            typed!(e2, Type::Float);
            Ok(Type::Float)
        },
        Syntax::CompBin(_, ref e1, ref e2) => {
            try!(unify(&try!(invoke!(env, e1)), &try!(invoke!(env, e2)),
                       extenv, tyenv));
            Ok(Type::Bool)
        },
        Syntax::If(ref e1, ref e2, ref e3) => {
            typed!(e1, Type::Bool);
            let t2 = try!(invoke!(env, e2));
            let t3 = try!(invoke!(env, e3));
            try!(unify(&t2, &t3, extenv, tyenv));
            Ok(t2)
        },
        Syntax::Let((ref x, ref t), ref e1, ref e2) => {
            typed!(e1, t);
            let mut cp_env = env.clone();
            cp_env.insert(x.to_string(), t.clone());
            invoke!(&cp_env, e2)
        },
        Syntax::Var(ref x) => {
            if let Some(t) = env.get(x).cloned() {
                Ok(t)
            } else if let Some(t) = extenv.get(x).cloned() {
                Ok(t)
            } else { // Inference of an external function
                println!("free variable {} assumed as external@.", x);
                let t = id_gen.gen_type();
                extenv.insert(x.to_string(), t.clone());
                Ok(t)
            }
        },
        Syntax::LetRec(ref fundef, ref e2) => {
            let (x, t) = fundef.name.clone();
            let yts = &fundef.args;
            let e1 = fundef.body.clone();
            let mut cp_env = env.clone();
            cp_env.insert(x, t.clone());
            let mut cp_env_body = cp_env.clone();
            for &(ref x, ref t) in yts.iter() {
                cp_env_body.insert(x.to_string(), t.clone());
            }
            try!(unify(&t,
                       &Type::Fun(yts.iter()
                                  .map(|xt| xt.1.clone())
                                  .collect::<Vec<_>>()
                                  .into_boxed_slice(), // List.map snd yts
                                 Box::new(try!(invoke!(&cp_env_body, &e1)))),
                       extenv, tyenv));
            invoke!(&cp_env, e2)
        },
        Syntax::App(ref e, ref es) => {
            let t = id_gen.gen_type();
            let funtype = Type::Fun(g_seq!(es), // List.map (g env) es
                                    Box::new(t.clone()));
            typed!(e, funtype);
            Ok(t)
        },
        Syntax::Tuple(ref es) => Ok(Type::Tuple(g_seq!(es))),
        Syntax::LetTuple(ref xts, ref e1, ref e2) => {
            typed!(e1, Type::Tuple(xts.iter()
                                  .map(|xt| xt.1.clone())
                                  .collect::<Vec<_>>()
                                   .into_boxed_slice())); // List.map snd xts
            let mut cp_env = env.clone();
            for &(ref x, ref t) in xts.iter() {
                cp_env.insert(x.to_string(), t.clone());
            }
            invoke!(&cp_env, e2)
        },
        Syntax::Array(ref e1, ref e2) => {
            typed!(e2, Type::Int);
            let t = try!(invoke!(env, e1));
            Ok(Type::Array(Box::new(t)))
        }
        Syntax::Get(ref e1, ref e2) => {
            let t = id_gen.gen_type();
            typed!(e1, Type::Array(Box::new(t.clone())));
            typed!(e2, Type::Int);
            Ok(t)
        }
        Syntax::Put(ref e1, ref e2, ref e3) => {
            let t = try!(invoke!(env, e3));
            typed!(e1, Type::Array(Box::new(t.clone())));
            typed!(e2, Type::Int);
            Ok(t)
        },
    }
}

/*
 * Type-check a given AST e.
 * Returns the resulting AST and extenv (environment of external functions).
 */
pub fn f(e: &Syntax, id_gen: &mut IdGen)
         -> Result<(Syntax, HashMap<String, Type>), String> {
    let mut extenv = HashMap::new();
    let mut tyenv = HashMap::new();
    let typed = g(&HashMap::new(), e, &mut extenv, &mut tyenv, id_gen)
        .unwrap();
    println!("{:?}", typed);
    match unify(&Type::Unit, &typed, &mut extenv, &mut tyenv) {
        Err(TypingError::Unify(_, _)) =>
            return Err("top level does not have type unit".to_string()),
        Ok(()) => {},
    }
    extenv = extenv.into_iter().map(|(k, x)| (k, deref_typ(&x, &tyenv))).collect();
    Ok((deref_term(e, &tyenv), extenv))
}

#[cfg(test)]
mod tests{
    use typing::*;
    use syntax::*;
    #[test]
    fn test_typing_add() {
        use self::Syntax::*;
        let mut id_gen = IdGen::new();
        let syn = IntBin(self::IntBin::Add,
                         Box::new(Int(14)),
                         Box::new(Int(23)));
        assert!(f(&syn, &mut id_gen).is_err()); // top is not :unit.
    }
    #[test]
    fn test_typing_ext_print() {
        use self::Syntax::*;
        let mut id_gen = IdGen::new();
        let syn = App(Box::new(Var("print_int".to_string())),
                      vec![Int(23)].into_boxed_slice());
        assert!(f(&syn, &mut id_gen).is_ok()); // top can be :unit.
    }
    #[test]
    fn test_typing_letrec() {
        use self::Syntax::*;
        let mut id_gen = IdGen::new();
        // let rec f x = f x in f 0
        let fundef = LetRec(Fundef {
            name: ("f".to_string(), Type::Var(100)),
            args: vec![("x".to_string(), Type::Var(101))].into_boxed_slice(),
            body: Box::new(App(Box::new(Var("f".to_string())), vec![Var("x".to_string())].into_boxed_slice()))
        }, Box::new(App(Box::new(Var("f".to_string())), vec![Int(0)].into_boxed_slice())));
        assert!(f(&fundef, &mut id_gen).is_ok());
    }
    #[test]
    fn test_typing_tuple() {
        use self::Syntax::*;
        let mut id_gen = IdGen::new();
        let tuple = Tuple(vec![Int(4), Int(5)]
                          .into_boxed_slice()); // (4, 5)
        // let (x: int, y: 'a100) = (...) in x
        // 100 is used to avoid collision with generated types
        let let_ex = LetTuple(vec![("x".to_string(), Type::Int),
                                   ("y".to_string(), Type::Var(100))]
                              .into_boxed_slice(),
                              Box::new(tuple),
                              Box::new(Var("x".to_string())));
        let printer = App(Box::new(Var("print_int".to_string())),
                      vec![let_ex].into_boxed_slice()); // print_int (...)
        assert!(f(&printer, &mut id_gen).is_ok());
    }
    #[test]
    fn test_typing_array() {
        use self::Syntax::*;
        let mut id_gen = IdGen::new();
        let ary = Array(Box::new(Int(4)), Box::new(Int(5))); // newarray 4 5
        let access = Get(Box::new(ary), Box::new(Int(2))); // (...).(2)
        let printer = App(Box::new(Var("print_int".to_string())),
                      vec![access].into_boxed_slice());
        assert!(f(&printer, &mut id_gen).is_ok());
    }
    #[test]
    fn test_deref_typ() {
        let ty = Type::Array(Box::new(Type::Var(2)));
        let tyenv = vec![(2, Type::Int)].into_iter().collect();
        assert_eq!(deref_typ(&ty, &tyenv), Type::Array(Box::new(Type::Int)));
    }
    #[test]
    fn test_deref_typ_unresolved() {
        let ty = Type::Array(Box::new(Type::Var(2)));
        let tyenv = vec![(0, Type::Float)].into_iter().collect();
        assert_eq!(deref_typ(&ty, &tyenv), Type::Array(Box::new(Type::Int)));
    }
}
