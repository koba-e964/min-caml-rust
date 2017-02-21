use syntax::*;
use std::collections::HashMap;

/* Type that represents an exception */
#[derive(Debug)]
enum TypingError {
    NotFound,
    Unify(Type, Type),
}

fn deref_typ(e: &Type) -> Type {
    panic!();
}

fn deref_term(e: &Syntax) -> Syntax {
    panic!();
}

/*
 * Checks if type variable n occurs in ty.
 */
fn occur(n: usize, ty: &Type) -> bool {
    false // TODO
}

fn unify(t1: &Type, t2: &Type,
         extenv: &mut HashMap<String, Type>) -> Result<(), TypingError> {
    println!("unify {:?} {:?}", t1, t2);
    macro_rules! unify_seq {
        ($t1s:expr, $t2s: expr) => {
            let n = $t1s.len();
            let m = $t2s.len();
            if n != m {
                return Err(TypingError::Unify(t1.clone(), t2.clone()));
            }
            for i in 0 .. n {
                try!(unify(&$t1s[i], &$t2s[i], extenv));
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
            unify(t1cod, t2cod, extenv)
        },
        (Type::Tuple(ref t1s), Type::Tuple(ref t2s)) => {
            unify_seq!(t1s, t2s);
            Ok(())
        },
        (Type::Array(ref t1), Type::Array(ref t2)) => unify(t1, t2, extenv),
        (Type::Var(n1), Type::Var(n2)) if n1 == n2 => Ok(()),
        (Type::Var(n1), _) => {
            if occur(n1, t2) {
                return Err(TypingError::Unify(t1.clone(), t2.clone()));
            }
            // TODO add constraint
            Ok(())
        },
        (_, Type::Var(n2)) => unify(t2, t1, extenv),
        _ => Err(TypingError::Unify(t1.clone(), t2.clone())),
    }
}


fn g(env: &HashMap<String, Type>, e: &Syntax,
     extenv: &mut HashMap<String, Type>) -> Result<Type, TypingError> {
    macro_rules! typed {
        ($e:expr, $ty:expr) => (try!(unify(&$ty, &try!(g(env, $e, extenv)), extenv)));
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
            try!(unify(&try!(g(env, e1, extenv)), &try!(g(env, e2, extenv)), extenv));
            Ok(Type::Bool)
        },
        Syntax::If(ref e1, ref e2, ref e3) => {
            typed!(e1, Type::Bool);
            let t2 = try!(g(env, e2, extenv));
            let t3 = try!(g(env, e3, extenv));
            try!(unify(&t2, &t3, extenv));
            Ok(t2)
        },
        Syntax::Let((ref x, ref t), ref e1, ref e2) => {
            typed!(e1, t);
            g(panic!(), e2, extenv)
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
            let t = try!(g(env, e3, extenv));
            typed!(e1, Type::Array(Box::new(t.clone())));
            typed!(e2, Type::Int);
            Ok(t)
        },
    }
} // TODO 8 cases left


pub fn f(e: &Syntax) -> Result<Syntax, String> {
    let mut extenv = HashMap::new();
    let typed = g(&HashMap::new(), e, &mut extenv).unwrap();
    println!("{:?}", typed);
    match unify(&Type::Unit, &typed, &mut extenv) {
        Err(TypingError::Unify(_, _)) =>
            return Err("top level does not have type unit".to_string()),
        Err(e) => return Err(format!("{:?}", e)),
        Ok(()) => {},
    }
    extenv = extenv.into_iter().map(|(k, x)| (k, deref_typ(&x))).collect();
    Ok(deref_term(e))
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
}
