use syntax::*;
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
    NotFound,
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
                t.clone()
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
        ($e:expr) => (deref_term(e, tyenv));
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
            panic!()
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
} // TODO 1 case left

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
        ($t1:expr, $t2:expr) => (unify(t1, t2, extenv, tyenv));
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
                invoke!(t1sub, t2)
            } else {
                if occur(*n1, t2) {
                    return Err(TypingError::Unify(t1.clone(), t2.clone()));
                }
                tyenv.insert(*n1, t2.clone());
                Ok(())
            }
        },
        (_, Type::Var(ref n2)) => invoke!(t2, t1),
        _ => Err(TypingError::Unify(t1.clone(), t2.clone())),
    }
}


fn g(env: &HashMap<String, Type>, e: &Syntax,
     extenv: &mut HashMap<String, Type>,
     tyenv: &mut HashMap<usize, Type>) -> Result<Type, TypingError> {
    macro_rules! invoke {
        ($env:expr, $e:expr) => (g($env, $e, extenv, tyenv));
    }
    macro_rules! typed {
        ($e:expr, $ty:expr) => (try!(unify(&$ty, &try!(invoke!(env, $e)), extenv, tyenv)));
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
            invoke!(panic!(), e2)
        },
        Syntax::Var(ref x) => {
            panic!()
        },
        Syntax::LetRec(ref fundef, ref e2) => {
            panic!()
        },
        Syntax::App(ref e, ref es) => {
            panic!()
        },
        Syntax::Tuple(ref es) => {
            panic!()
        }
        Syntax::LetTuple(ref xts, ref e1, ref e2) => {
            panic!()
        },
        Syntax::Array(ref e1, ref e2) => {
            panic!()
        }
        Syntax::Get(ref e1, ref e2) => {
            panic!()
        }
        Syntax::Put(ref e1, ref e2, ref e3) => {
            let t = try!(invoke!(env, e3));
            typed!(e1, Type::Array(Box::new(t.clone())));
            typed!(e2, Type::Int);
            Ok(t)
        },
    }
} // TODO 8 cases left


pub fn f(e: &Syntax) -> Result<Syntax, String> {
    let mut extenv = HashMap::new();
    let mut tyenv = HashMap::new();
    let typed = g(&HashMap::new(), e, &mut extenv, &mut tyenv).unwrap();
    println!("{:?}", typed);
    match unify(&Type::Unit, &typed, &mut extenv, &mut tyenv) {
        Err(TypingError::Unify(_, _)) =>
            return Err("top level does not have type unit".to_string()),
        Err(e) => return Err(format!("{:?}", e)),
        Ok(()) => {},
    }
    extenv = extenv.into_iter().map(|(k, x)| (k, deref_typ(&x, &tyenv))).collect();
    Ok(deref_term(e, &tyenv))
}

#[cfg(test)]
mod tests{
    use typing::*;
    #[test]
    fn test_typing_add() {
        use self::Syntax::*;
        let syn = IntBin(self::IntBin::Add,
                         Box::new(Int(14)),
                         Box::new(Int(23)));
        assert!(f(&syn).is_err()); // top is not :unit.
    }
    #[test]
    fn test_typing_ext_print() {
        use self::Syntax::*;
        let syn = App(Box::new(Var("print_int".to_string())),
                      vec![Int(23)].into_boxed_slice());
        assert!(f(&syn).is_ok()); // top can be :unit.
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
