use crate::id::IdGen;
use crate::syntax::{Fundef, Syntax, Type};
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
pub enum TypingError {
    Unify(Type, Type),
}

fn deref_typ(e: &Type, tyenv: &HashMap<usize, Type>) -> Type {
    macro_rules! deref_typ_list {
        ($ls:expr) => {
            $ls.iter()
                .map(|x| deref_typ(x, tyenv))
                .collect::<Vec<_>>()
                .into_boxed_slice()
        };
    }
    match e {
        Type::Fun(t1s, t2) => Type::Fun(deref_typ_list!(t1s), Box::new(deref_typ(t2, tyenv))),
        Type::Tuple(ts) => Type::Tuple(deref_typ_list!(ts)),
        Type::Array(t) => Type::Array(Box::new(deref_typ(t, tyenv))),
        Type::Var(n) => {
            if let Some(t) = tyenv.get(n) {
                deref_typ(t, tyenv)
            } else {
                println!(
                    "uninstantiated type variable {} detected; assuming int@.",
                    n
                );
                Type::Int
            }
        }
        _ => e.clone(),
    }
}

fn deref_term(e: &Syntax, tyenv: &HashMap<usize, Type>) -> Syntax {
    macro_rules! invoke {
        ($e:expr) => {
            deref_term($e, tyenv)
        };
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
        ($constr:expr, $op:expr, $e1:expr, $e2:expr) => {{
            $constr(*$op, Box::new(invoke!($e1)), Box::new(invoke!($e2)))
        }};
    }
    macro_rules! boxed_array {
        ($ls:expr) => {
            $ls.iter()
                .map(|x| invoke!(x))
                .collect::<Vec<_>>()
                .into_boxed_slice()
        };
    }
    match e {
        Syntax::Not(e) => pack!(Syntax::Not, e),
        Syntax::Neg(e) => pack!(Syntax::Neg, e),
        Syntax::IntBin(op, e1, e2) => pack_bin!(Syntax::IntBin, op, e1, e2),
        Syntax::FNeg(e) => pack!(Syntax::FNeg, e),
        Syntax::FloatBin(op, e1, e2) => pack_bin!(Syntax::FloatBin, op, e1, e2),
        Syntax::CompBin(op, e1, e2) => pack_bin!(Syntax::CompBin, op, e1, e2),
        Syntax::If(e1, e2, e3) => pack!(Syntax::If, e1, e2, e3),
        Syntax::Let((x, t), e1, e2) => Syntax::Let(
            (x.clone(), deref_typ(t, tyenv)),
            Box::new(invoke!(e1)),
            Box::new(invoke!(e2)),
        ),
        Syntax::LetRec(fundef, e2) => {
            let (x, t) = &fundef.name;
            let yts = &fundef.args;
            let e1 = &fundef.body;
            Syntax::LetRec(
                Fundef {
                    name: (x.to_string(), deref_typ(t, tyenv)),
                    args: yts
                        .iter()
                        .map(|(x, t)| (x.clone(), deref_typ(t, tyenv)))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                    body: Box::new(invoke!(e1)),
                },
                Box::new(invoke!(e2)),
            )
        }
        Syntax::App(e, es) => Syntax::App(Box::new(invoke!(e)), boxed_array!(es)),
        Syntax::Tuple(es) => Syntax::Tuple(boxed_array!(es)),
        Syntax::LetTuple(xts, e1, e2) => {
            let xts: Box<_> = xts
                .iter()
                .map(|(x, t)| (x.clone(), deref_typ(t, tyenv)))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            Syntax::LetTuple(xts, Box::new(invoke!(e1)), Box::new(invoke!(e2)))
        }
        Syntax::Array(e1, e2) => pack!(Syntax::Array, e1, e2),
        Syntax::Get(e1, e2) => pack!(Syntax::Get, e1, e2),
        Syntax::Put(e1, e2, e3) => pack!(Syntax::Put, e1, e2, e3),
        _ => e.clone(),
    }
}

/*
 * Checks if type variable r1 occurs in ty.
 */
fn occur(r1: usize, ty: &Type) -> bool {
    macro_rules! occur_list {
        ($ls:expr) => {
            $ls.iter().any(|ty| occur(r1, ty))
        };
    }
    match ty {
        Type::Fun(t2s, t2) => occur_list!(t2s) || occur(r1, t2),
        Type::Tuple(t2s) => occur_list!(t2s),
        Type::Array(t2) => occur(r1, t2),
        Type::Var(r2) => r1 == *r2,
        _ => false,
    }
}

fn unify(
    t1: &Type,
    t2: &Type,
    _extenv: &mut HashMap<String, Type>,
    tyenv: &mut HashMap<usize, Type>,
) -> Result<(), TypingError> {
    println!("unify {:?} {:?}", t1, t2);
    macro_rules! invoke {
        ($t1:expr, $t2:expr) => {
            unify($t1, $t2, _extenv, tyenv)
        };
    }
    macro_rules! unify_seq {
        ($t1s:expr, $t2s: expr) => {
            let n = $t1s.len();
            let m = $t2s.len();
            if n != m {
                return Err(TypingError::Unify(t1.clone(), t2.clone()));
            }
            for i in 0..n {
                invoke!(&$t1s[i], &$t2s[i])?;
            }
        };
    }
    match (t1, t2) {
        (Type::Unit, Type::Unit) => Ok(()),
        (Type::Bool, Type::Bool) => Ok(()),
        (Type::Int, Type::Int) => Ok(()),
        (Type::Float, Type::Float) => Ok(()),
        (Type::Fun(t1s, t1cod), Type::Fun(t2s, t2cod)) => {
            unify_seq!(t1s, t2s);
            invoke!(t1cod, t2cod)
        }
        (Type::Tuple(t1s), Type::Tuple(t2s)) => {
            unify_seq!(t1s, t2s);
            Ok(())
        }
        (Type::Array(t1), Type::Array(t2)) => invoke!(t1, t2),
        (Type::Var(n1), Type::Var(n2)) if n1 == n2 => Ok(()),
        (Type::Var(n1), _) => {
            if let Some(t1sub) = tyenv.get(n1).cloned() {
                invoke!(&t1sub, t2)
            } else {
                if occur(*n1, t2) {
                    return Err(TypingError::Unify(t1.clone(), t2.clone()));
                }
                tyenv.insert(*n1, t2.clone());
                Ok(())
            }
        }
        (_, Type::Var(_)) => invoke!(t2, t1),
        _ => Err(TypingError::Unify(t1.clone(), t2.clone())),
    }
}

fn g(
    env: &HashMap<String, Type>,
    e: &Syntax,
    extenv: &mut HashMap<String, Type>,
    tyenv: &mut HashMap<usize, Type>,
    id_gen: &mut IdGen,
) -> Result<Type, TypingError> {
    macro_rules! invoke {
        ($env:expr, $e:expr) => {
            g($env, $e, extenv, tyenv, id_gen)
        };
    }
    macro_rules! typed {
        ($e:expr, $ty:expr) => {
            unify(&$ty, &invoke!(env, $e)?, extenv, tyenv)?
        };
    }
    /* map (g env) to an &[Syntax]
     * &[Syntax] -> Box<[Type]>
     */
    macro_rules! g_seq {
        ($es:expr) => {{
            let mut argtype = Vec::new();
            for e in $es.iter() {
                argtype.push(invoke!(env, e)?);
            }
            argtype.into_boxed_slice()
        }};
    }
    match e {
        Syntax::Unit => Ok(Type::Unit),
        Syntax::Bool(_) => Ok(Type::Bool),
        Syntax::Int(_) => Ok(Type::Int),
        Syntax::Float(_) => Ok(Type::Float),
        Syntax::Not(e) => {
            typed!(e, Type::Bool);
            Ok(Type::Bool)
        }
        Syntax::Neg(e) => {
            typed!(e, Type::Int);
            Ok(Type::Int)
        }
        Syntax::IntBin(_, e1, e2) => {
            typed!(e1, Type::Int);
            typed!(e2, Type::Int);
            Ok(Type::Int)
        }
        Syntax::FNeg(e) => {
            typed!(e, Type::Float);
            Ok(Type::Float)
        }
        Syntax::FloatBin(_, e1, e2) => {
            typed!(e1, Type::Float);
            typed!(e2, Type::Float);
            Ok(Type::Float)
        }
        Syntax::CompBin(_, e1, e2) => {
            unify(&invoke!(env, e1)?, &invoke!(env, e2)?, extenv, tyenv)?;
            Ok(Type::Bool)
        }
        Syntax::If(e1, e2, e3) => {
            typed!(e1, Type::Bool);
            let t2 = invoke!(env, e2)?;
            let t3 = invoke!(env, e3)?;
            unify(&t2, &t3, extenv, tyenv)?;
            Ok(t2)
        }
        Syntax::Let((x, t), e1, e2) => {
            typed!(e1, t);
            let mut cp_env = env.clone();
            cp_env.insert(x.to_string(), t.clone());
            invoke!(&cp_env, e2)
        }
        Syntax::Var(x) => {
            if let Some(t) = env.get(x).cloned() {
                Ok(t)
            } else if let Some(t) = extenv.get(x).cloned() {
                Ok(t)
            } else {
                // Inference of an external function
                println!("free variable {} assumed as external@.", x);
                let t = id_gen.gen_type();
                extenv.insert(x.to_string(), t.clone());
                Ok(t)
            }
        }
        Syntax::LetRec(fundef, e2) => {
            let (x, t) = fundef.name.clone();
            let yts = &fundef.args;
            let e1 = fundef.body.clone();
            let mut cp_env = env.clone();
            cp_env.insert(x, t.clone());
            let mut cp_env_body = cp_env.clone();
            for (x, t) in yts.iter() {
                cp_env_body.insert(x.to_string(), t.clone());
            }
            unify(
                &t,
                &Type::Fun(
                    yts.iter()
                        .map(|xt| xt.1.clone())
                        .collect::<Vec<_>>()
                        .into_boxed_slice(), // List.map snd yts
                    Box::new(invoke!(&cp_env_body, &e1)?),
                ),
                extenv,
                tyenv,
            )?;
            invoke!(&cp_env, e2)
        }
        Syntax::App(e, es) => {
            let t = id_gen.gen_type();
            let funtype = Type::Fun(
                g_seq!(es), // List.map (g env) es
                Box::new(t.clone()),
            );
            typed!(e, funtype);
            Ok(t)
        }
        Syntax::Tuple(es) => Ok(Type::Tuple(g_seq!(es))),
        Syntax::LetTuple(xts, e1, e2) => {
            typed!(
                e1,
                Type::Tuple(
                    xts.iter()
                        .map(|xt| xt.1.clone())
                        .collect::<Vec<_>>()
                        .into_boxed_slice()
                )
            ); // List.map snd xts
            let mut cp_env = env.clone();
            for (x, t) in xts.iter() {
                cp_env.insert(x.to_string(), t.clone());
            }
            invoke!(&cp_env, e2)
        }
        Syntax::Array(e1, e2) => {
            typed!(e1, Type::Int);
            let t = invoke!(env, e2)?;
            Ok(Type::Array(Box::new(t)))
        }
        Syntax::Get(e1, e2) => {
            let t = id_gen.gen_type();
            typed!(e1, Type::Array(Box::new(t.clone())));
            typed!(e2, Type::Int);
            Ok(t)
        }
        Syntax::Put(e1, e2, e3) => {
            let t = invoke!(env, e3)?;
            typed!(e1, Type::Array(Box::new(t.clone())));
            typed!(e2, Type::Int);
            Ok(Type::Unit)
        }
    }
}

/*
 * Type-check a given AST e.
 * Returns the resulting AST and extenv (environment of external functions).
 * If this function succeeds, an AST wrapped with Ok is returned and extenv is
 * updated.
 * If this function fails, an error message wrapped with Err is returned.
 * The content of extenv is unspecified.
 */
pub fn f(
    e: &Syntax,
    id_gen: &mut IdGen,
    extenv: &mut HashMap<String, Type>,
) -> Result<Syntax, String> {
    let mut tyenv = HashMap::new();
    let typed = g(&HashMap::new(), e, extenv, &mut tyenv, id_gen).unwrap();
    println!("{:?}", typed);
    match unify(&Type::Unit, &typed, extenv, &mut tyenv) {
        Err(TypingError::Unify(_, _)) => {
            return Err("top level does not have type unit".to_string())
        }
        Ok(()) => {}
    }
    *extenv = extenv
        .iter()
        .map(|(k, x)| (k.clone(), deref_typ(x, &tyenv)))
        .collect();
    Ok(deref_term(e, &tyenv))
}

#[cfg(test)]
mod tests {
    use crate::syntax::*;
    use crate::typing::*;
    use std::collections::HashMap;
    #[test]
    fn test_typing_add() {
        use super::Syntax::*;
        let mut id_gen = IdGen::new();
        let mut extenv = HashMap::new();
        let syn = IntBin(self::IntBin::Add, Box::new(Int(14)), Box::new(Int(23)));
        assert!(f(&syn, &mut id_gen, &mut extenv).is_err()); // top is not :unit.
    }
    #[test]
    fn test_typing_ext_print() {
        use super::Syntax::*;
        let mut id_gen = IdGen::new();
        let mut extenv = HashMap::new();
        let syn = App(Box::new(Var("print_int".to_string())), Box::new([Int(23)]));
        assert!(f(&syn, &mut id_gen, &mut extenv).is_ok()); // top can be :unit.
    }
    #[test]
    fn test_typing_letrec() {
        use super::Syntax::*;
        let mut id_gen = IdGen::new();
        let mut extenv = HashMap::new();
        // let rec f x = f x in f 0
        let fundef = LetRec(
            Fundef {
                name: ("f".to_string(), Type::Var(100)),
                args: Box::new([("x".to_string(), Type::Var(101))]),
                body: Box::new(App(
                    Box::new(Var("f".to_string())),
                    Box::new([Var("x".to_string())]),
                )),
            },
            Box::new(App(Box::new(Var("f".to_string())), Box::new([Int(0)]))),
        );
        assert!(f(&fundef, &mut id_gen, &mut extenv).is_ok());
    }
    #[test]
    fn test_typing_tuple() {
        use self::Syntax::*;
        let mut id_gen = IdGen::new();
        let mut extenv = HashMap::new();
        let tuple = Tuple(Box::new([Int(4), Int(5)])); // (4, 5)
                                                       // let (x: int, y: 'a100) = (...) in x
                                                       // 100 is used to avoid collision with generated types
        let let_ex = LetTuple(
            Box::new([
                ("x".to_string(), Type::Int),
                ("y".to_string(), Type::Var(100)),
            ]),
            Box::new(tuple),
            Box::new(Var("x".to_string())),
        );
        let printer = App(Box::new(Var("print_int".to_string())), Box::new([let_ex])); // print_int (...)
        assert!(f(&printer, &mut id_gen, &mut extenv).is_ok());
    }
    #[test]
    fn test_typing_array() {
        use super::Syntax::*;
        let mut id_gen = IdGen::new();
        let mut extenv = HashMap::new();
        let ary = Array(Box::new(Int(4)), Box::new(Int(5))); // newarray 4 5
        let access = Get(Box::new(ary), Box::new(Int(2))); // (...).(2)
        let printer = App(Box::new(Var("print_int".to_string())), Box::new([access]));
        assert!(f(&printer, &mut id_gen, &mut extenv).is_ok());
    }
    #[test]
    fn test_typing_extenv() {
        use super::Syntax::*;
        let mut id_gen = IdGen::new();
        let test = || "test".to_string();
        let mut extenv = [(test(), Type::Unit)].iter().cloned().collect();
        let syn = Var(test());
        assert!(f(&syn, &mut id_gen, &mut extenv).is_ok());
    }
    #[test]
    fn test_typing_extenv_negative() {
        use self::Syntax::*;
        let mut id_gen = IdGen::new();
        let test = || "test".to_string();
        let mut extenv = [(test(), Type::Int)].iter().cloned().collect();
        let syn = Var(test());
        assert!(f(&syn, &mut id_gen, &mut extenv).is_err()); // top-level type is not unit
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
