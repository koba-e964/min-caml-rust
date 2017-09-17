use ordered_float::OrderedFloat;
use syntax::{IntBin, FloatBin, CompBin, Syntax, Type, Fundef};
use id::{IdGen};
use std::collections::{HashMap, HashSet};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KNormal {
    Unit,
    Int(i64),
    Float(OrderedFloat<f64>),
    Neg(String),
    IntBin(IntBin, String, String),
    FNeg(String),
    FloatBin(FloatBin, String, String),
    IfComp(CompBin, String, String, Box<KNormal>, Box<KNormal>),
    Let((String, Type), Box<KNormal>, Box<KNormal>),
    Var(String),
    LetRec(KFundef, Box<KNormal>),
    App(String, Box<[String]>),
    Tuple(Box<[String]>),
    LetTuple(Box<[(String, Type)]>, String, Box<KNormal>),
    Get(String, String),
    Put(String, String, String),
    ExtArray(String),
    ExtFunApp(String, Box<[String]>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KFundef {
    pub name: (String, Type),
    pub args: Box<[(String, Type)]>,
    pub body: Box<KNormal>,
}

// Display
impl KNormal {
    fn fmt2(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        use self::KNormal::*;
        match *self {
            Unit => write!(f, "()"),
            Int(v) => write!(f, "{}", v),
            Float(fv) => write!(f, "{}", fv),
            Neg(ref x) => write!(f, "-{}", x),
            IntBin(op, ref x, ref y) => {
                let op_str = match op {
                    self::IntBin::Add => "+",
                    self::IntBin::Sub => "-",
                };
                write!(f, "{} {} {}", x, op_str, y)
            },
            FNeg(ref x) => write!(f, "-.{}", x),
            FloatBin(op, ref x, ref y) => {
                let op_str = match op {
                    self::FloatBin::FAdd => "+.",
                    self::FloatBin::FSub => "-.",
                    self::FloatBin::FMul => "*.",
                    self::FloatBin::FDiv => "/.",
                };
                write!(f, "{} {} {}", x, op_str, y)
            },
            IfComp(op, ref x, ref y, ref e1, ref e2) => {
                let op_str = match op {
                    self::CompBin::Eq => "=",
                    self::CompBin::LE => "<=",
                };
                write!(f, "if {} {} {} then\n", x, op_str, y)?;
                for _ in 0 .. level + 2 {
                    write!(f, " ")?;
                }
                e1.fmt2(f, level + 2)?;
                write!(f, "\n")?;
                for _ in 0 .. level {
                    write!(f, " ")?;
                }
                write!(f, "else\n")?;
                for _ in 0 .. level + 2 {
                    write!(f, " ")?;
                }
                e2.fmt2(f, level + 2)
            },
            Let((ref x, ref t), ref e1, ref e2) => {
                if let Type::Unit = *t {
                    if x.len() >= 6 && &x[0..6] == "_dummy" {
                        // this let expression is actually "e1; e2"
                        e1.fmt2(f, level)?;
                        write!(f, ";\n")?;
                        for _ in 0 .. level {
                            write!(f, " ")?;
                        }
                        return e2.fmt2(f, level);
                    }
                }
                write!(f, "let {}: {} = ", x, t)?;
                e1.fmt2(f, level)?;
                write!(f, " in\n")?;
                for _ in 0 .. level {
                    write!(f, " ")?;
                }
                e2.fmt2(f, level)
            },
            Var(ref x) => write!(f, "{}", x),
            LetRec(KFundef { name: (ref x, ref t), args: ref yts, body: ref e1 },
                   ref e2) => {
                write!(f, "let rec ({}: {})", x, t)?;
                for i in 0 .. yts.len() {
                    write!(f, " ({}: {})", yts[i].0, yts[i].1)?;
                }
                write!(f, " =\n")?;
                for _ in 0 .. level + 1 {
                    write!(f, " ")?;
                }
                e1.fmt2(f, level + 1)?;
                write!(f, " in\n")?;
                for _ in 0 .. level {
                    write!(f, " ")?;
                }
                e2.fmt2(f, level)
            }
            App(ref func, ref args) => {
                write!(f, "{}", func)?;
                for v in args.iter() {
                    write!(f, " {}", v)?;
                }
                Ok(())
            },
            Tuple(ref elems) => {
                write!(f, "(")?;
                for i in 0 .. elems.len() {
                    write!(f, "{}", elems[i])?;
                    if i < elems.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            },
            LetTuple(ref xts, ref y, ref e) => {
                write!(f, "let (")?;
                for i in 0 .. xts.len() {
                    write!(f, "{}: {}", xts[i].0, xts[i].1)?;
                    if i < xts.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") = {} in\n", y)?;
                for _ in 0 .. level {
                    write!(f, " ")?;
                }
                e.fmt2(f, level)
            },
            Get(ref x, ref y) =>
                write!(f, "{}.({})", x, y),
            Put(ref x, ref y, ref z) =>
                write!(f, "{}.({}) <- {}", x, y, z),
            ExtArray(ref a) => write!(f, "(extarr:{})", a),
            ExtFunApp(ref func, ref args) => {
                write!(f, "(ext:{})", func)?;
                for v in args.iter() {
                    write!(f, " {}", v)?;
                }
                Ok(())
            },
        }
    }
}

impl fmt::Display for KNormal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt2(f, 0)
    }
}

/*
 * Free variables in an AST, used in elim.rs
 */
pub fn fv(e: &KNormal) -> HashSet<String> {
    macro_rules! invoke {
        ($e:expr) => (fv($e));
    }
    macro_rules! build_set {
        ($($x:expr),*) => ({
            let mut h = HashSet::new();
            $(h.insert($x.clone());)*
            h
        })
    }
    match *e {
        KNormal::Unit | KNormal::Int(_) |
        KNormal::Float(_) | KNormal::ExtArray(_) => HashSet::new(),
        KNormal::Neg(ref x) | KNormal::FNeg(ref x) => build_set!(x),
        KNormal::IntBin(_, ref x, ref y) |
        KNormal::FloatBin(_, ref x, ref y) |
        KNormal::Get(ref x, ref y) => build_set!(x, y),
        KNormal::IfComp(_, ref x, ref y, ref e1, ref e2) => {
            let h = build_set!(x, y);
            let s1 = invoke!(e1);
            let s2 = invoke!(e2);
            &(&h | &s1) | &s2
        },
        KNormal::Let((ref x, _), ref e1, ref e2) => {
            let s1 = invoke!(e1);
            let s2 = &invoke!(e2) - &build_set!(x);
            &s1 | &s2
        }
        KNormal::Var(ref x) => build_set!(x),
        KNormal::LetRec(ref fundef, ref e2) => {
            let yts = &fundef.args;
            let e1 = &fundef.body;
            let (ref x, _) = fundef.name;
            let zs = &invoke!(e1) - &yts.iter().map(|x| x.0.clone()).collect();
            &(&zs | &invoke!(e2)) - &build_set!(x)
        },
        KNormal::App(ref x, ref ys) =>
            &build_set!(x) | &ys.iter().cloned().collect::<HashSet<_>>(),
        KNormal::Tuple(ref xs) => xs.iter().cloned().collect(),
        KNormal::LetTuple(ref xs, ref y, ref e) => {
            let tmp: HashSet<String> = xs.iter().map(|x| x.0.clone())
                .collect(); // S.of_list (List.map fst xs)
            &build_set!(y) | &(&invoke!(e) - &tmp)
        },
        KNormal::Put(ref x, ref y, ref z) => build_set!(x, y, z),
        KNormal::ExtFunApp(_, ref xs) => xs.iter().cloned().collect(),
    }
}


fn g(env: &HashMap<String, Type>, e: Syntax, id_gen: &mut IdGen, extenv: &HashMap<String, Type>)
     -> (KNormal, Type) {
    /*
     * Function insert_let is moved here, and realized as a macro.
     */
    macro_rules! insert_let_macro {
        ($et:expr, $k:expr) => ({
            let (e, t) = $et;
            match e {
                KNormal::Var(x) => $k(x),
                _ => {
                    let x = id_gen.gen_tmp(&t);
                    let (e2, t2) = $k(x.clone());
                    (KNormal::Let((x, t), Box::new(e), Box::new(e2)), t2)
                },
            }
        });
    }
    macro_rules! insert_let_macro_with_env {
        ($et:expr, $k:expr, $env:expr, $id_gen:expr, $extenv:expr) => ({
            let (e, t) = $et;
            match e {
                KNormal::Var(x) => $k(x),
                _ => {
                    let x = $id_gen.gen_tmp(&t);
                    let (e2, t2) = $k(x.clone());
                    (KNormal::Let((x, t), Box::new(e), Box::new(e2)), t2)
                },
            }
        });
    }
    macro_rules! invoke {
        ($e:expr) => (g(env, $e, id_gen, extenv));
    }
    // (Syntax, F) -> KNormal where F: Fn(String) -> (KNormal, Type)
    macro_rules! insert_let_helper {
        ($e:expr, $k:expr) => ({
            let e = invoke!($e);
            insert_let_macro!(e, $k)
        });
    }
    macro_rules! insert_let_helper_with_env {
        ($e:expr, $k:expr, $env:expr, $id_gen:expr, $extenv:expr) => ({
            let e = g($env, $e, $id_gen, $extenv);
            insert_let_macro_with_env!(e, $k, $env, $id_gen, $extenv)
        });
    }
    macro_rules! insert_let_binop {
        ($e1:expr, $e2:expr, $constr:expr, $ty:expr, $op:expr) => (
            insert_let_helper!(
                $e1, |x| insert_let_helper!(
                    ($e2).clone(),
                    |y| ($constr($op, x, y), $ty))));
    }

    match e {
        Syntax::Unit => (KNormal::Unit, Type::Unit),
        // bool is converted to int here
        Syntax::Bool(b) => (KNormal::Int(if b { 1 } else { 0 }), Type::Int),
        Syntax::Int(i) => (KNormal::Int(i), Type::Int),
        Syntax::Float(d) => (KNormal::Float(d), Type::Float),
        Syntax::Not(e) =>
            invoke!(Syntax::If(e,
                               Box::new(Syntax::Bool(false)),
                               Box::new(Syntax::Bool(true)))),
        Syntax::Neg(e) =>
            insert_let_helper!(*e, |x| (KNormal::Neg(x), Type::Int)),
        Syntax::IntBin(op, e1, e2) =>
            insert_let_binop!(*e1, *e2, KNormal::IntBin, Type::Int, op),
        Syntax::FNeg(e) =>
            insert_let_helper!(*e, |x| (KNormal::FNeg(x), Type::Float)),
        Syntax::FloatBin(op, e1, e2) =>
            insert_let_binop!(*e1, *e2, KNormal::FloatBin, Type::Float, op),
        Syntax::CompBin(_, _, _) =>
            invoke!(Syntax::If(Box::new(e),
                               Box::new(Syntax::Bool(true)),
                               Box::new(Syntax::Bool(false)))),
        Syntax::If(e1, e3, e4) => {
            let tmp = *e1; // http://stackoverflow.com/questions/28466809/collaterally-moved-error-on-deconstructing-box-of-pairs
            match tmp {
                Syntax::Not(e1) => invoke!(Syntax::If(e1, e4, e3)),
                Syntax::CompBin(op, e1, e2) =>
                    insert_let_helper!(
                        *e1,
                        |x| insert_let_helper!(
                            (*e2).clone(), // TODO maybe unnecessary cloning, but needed to squelch borrow checker
                            |y| {
                                let (e3p, t3) = invoke!((*e3).clone());
                                let (e4p, _t4) = invoke!((*e4).clone());
                                (KNormal::IfComp(op, x, y, Box::new(e3p),
                                                 Box::new(e4p)), t3)
                            })),
                _ => invoke!(Syntax::If(Box::new(Syntax::CompBin(CompBin::Eq, Box::new(tmp), Box::new(Syntax::Bool(false)))),
                                        e4, e3)),
            }
        },
        Syntax::Let((x, t), e1, e2) => {
            let (e1, _t1) = invoke!(*e1);
            let mut cp_env = env.clone();
            cp_env.insert(x.clone(), t.clone());
            let (e2, t2) = g(&cp_env, *e2, id_gen, extenv);
            (KNormal::Let((x, t), Box::new(e1), Box::new(e2)), t2)
        },
        Syntax::Var(x) => {
            if let Some(t) = env.get(&x).cloned() {
                return (KNormal::Var(x), t);
            }
            let ty = extenv.get(&x).unwrap().clone(); // It is guaranteed that there is.
            match ty {
                Type::Array(_) => (KNormal::ExtArray(x), ty),
                _ => panic!(format!("external variable {} does not have an array type", x)),
            }
        },
        Syntax::LetRec(fundef, e2) => {
            let Fundef {name: (x, t), args: yts, body: e1} = fundef;
            let mut cp_env = env.clone();
            cp_env.insert(x.clone(), t.clone());
            let (e2p, t2) = g(&cp_env, *e2, id_gen, extenv);
            for &(ref y, ref yt) in yts.iter() {
                cp_env.insert(y.clone(), yt.clone());
            }
            let (e1p, _t1) = g(&cp_env, *e1, id_gen, extenv);
            (KNormal::LetRec(
                KFundef { name: (x, t), args: yts, body: Box::new(e1p) },
                Box::new(e2p)),
             t2)
        },
        Syntax::App(e1, e2s) => {
            let mut extfunname = None;
            if let Syntax::Var(ref f) = *e1 {
                if !env.contains_key(f) && extenv.contains_key(f) {
                    extfunname = Some(f.to_string());
                }
            }
            if extfunname == None {
                let g_e1 = invoke!(*e1);
                match g_e1.1.clone() {
                    Type::Fun(_, t) => {
                        // TODO very rough way to simulate an internal recursive function in the original code
                        fn bind(mut xs: Vec<String>, p: usize, id_gen: &mut IdGen, extenv: &HashMap<String, Type>, env: &HashMap<String, Type>, e2s: &[Syntax], t: Type, f: String) -> (KNormal, Type) {
                            let n = e2s.len();
                            if p == n {
                                (KNormal::App(
                                    f.clone(),
                                    xs.clone().into_boxed_slice()),
                                 t.clone())
                            } else {
                                insert_let_helper_with_env!(e2s[p].clone(),
                                                            |x: String| {
                                                                xs.push(x.clone());
                                                                bind(xs, p + 1, id_gen, extenv, env, e2s, t, f)
                                                            }, env, id_gen, extenv)
                            }
                        }
                        insert_let_macro!(
                            g_e1.clone(),
                            |f: String|
                            bind(Vec::new(), 0, id_gen, extenv, env,
                                 &e2s, (*t).clone(), f))
                    },
                    _ => panic!(),
                }
            } else { // extfunname = Some(_)
                let extfunname = extfunname.unwrap();
                match extenv.get(&extfunname).unwrap().clone() {
                    Type::Fun(_, t) => {
                        // TODO very rough way to simulate an internal recursive function in the original code
                        fn bind(mut xs: Vec<String>, p: usize, id_gen: &mut IdGen, extenv: &HashMap<String, Type>, env: &HashMap<String, Type>, e2s: &[Syntax], t: Type, f: String) -> (KNormal, Type) {
                            let n = e2s.len();
                            if p == n {
                                (KNormal::ExtFunApp(
                                    f.clone(),
                                    xs.clone().into_boxed_slice()),
                                 t.clone())
                            } else {
                                insert_let_helper_with_env!(e2s[p].clone(),
                                                            |x: String| {
                                                                xs.push(x.clone());
                                                                bind(xs, p + 1, id_gen, extenv, env, e2s, t, f)
                                                            }, env, id_gen, extenv)
                            }
                        }
                        bind(Vec::new(), 0, id_gen, extenv, env,
                             &e2s, (*t).clone(), extfunname)
                    },
                    _ => panic!(),
                }
            }
        },
        Syntax::Tuple(es) => {
            fn bind(mut xs: Vec<String>, mut ts: Vec<Type>, p: usize,
                    env: &HashMap<String, Type>, id_gen: &mut IdGen,
                    extenv: &HashMap<String, Type>, es: &[Syntax])
                    -> (KNormal, Type) {
                let n = es.len();
                if p == n {
                    (KNormal::Tuple(xs.into_boxed_slice()),
                     Type::Tuple(ts.into_boxed_slice()))
                } else {
                    let (ex, tx) = g(env, es[p].clone(), id_gen, extenv); 
                    insert_let_macro_with_env!((ex, tx.clone()),
                                                |x: String| {
                                                    xs.push(x.clone());
                                                    ts.push(tx);
                                                    bind(xs, ts, p + 1,
                                                         env, id_gen, extenv,
                                                         es)
                                                }, env, id_gen, extenv)
                }
            }
            bind(Vec::new(), Vec::new(), 0, env, id_gen, extenv, &es)
        },
        Syntax::LetTuple(xts, e1, e2) => {
            insert_let_helper!(*e1,
                               move |y| {
                                   let mut cp_env = env.clone();
                                   for &(ref x, ref t) in xts.iter() {
                                       cp_env.insert(x.clone(), t.clone());
                                   }
                                   let (e2p, t2) = g(&cp_env, *e2, id_gen,
                                                     extenv);
                                   (KNormal::LetTuple(xts, y, Box::new(e2p)),
                                    t2)
                               }
            )
        },
        Syntax::Array(e1, e2) => {
            insert_let_helper!(*e1,
                               move |x| {
                                   let g_e2 = invoke!((*e2).clone());
                                   let t2 = g_e2.1.clone();
                                   insert_let_macro!(g_e2,
                                                     move |y| {
                                                         let l = match t2.clone() {
                                                             Type::Float => "create_float_array",
                                                             _ => "create_array",
                                                         }.to_string();
                                                         (KNormal::ExtFunApp(l, Box::new([x, y])), Type::Array(Box::new(t2)))
                                                     })
                               })
        },
        Syntax::Get(e1, e2) => {
            let g_e1 = invoke!(*e1);
            match g_e1.1.clone() {
                Type::Array(ref t) => insert_let_macro!(
                    g_e1,
                    |x| insert_let_helper!(
                        (*e2).clone(), |y| (KNormal::Get(x, y), (t as &Type).clone()))),
                _ => panic!("e1 should be an array")
            }
        },
        Syntax::Put(e1, e2, e3) => {
            insert_let_helper!(
                *e1, |x| insert_let_helper!(
                    (*e2).clone(), |y| insert_let_helper!(
                        (*e3).clone(), |z|
                        (KNormal::Put(x, y, z), Type::Unit))))
                
        },
    }
}


pub fn f(e: Syntax, id_gen: &mut IdGen, extenv: &HashMap<String, Type>)
         -> (KNormal, Type) {
    g(&HashMap::new(), e, id_gen, extenv)
}


#[cfg(test)]
mod tests {
    use syntax::*;
    use k_normal::*;
    #[test]
    fn test_fv_let() {
        use self::KNormal::*;
        // let x: int = y + y in z - x, fv(...) = {y, z}
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = || "z".to_string();
        let expr = Let((x(), Type::Int),
                       Box::new(IntBin(::syntax::IntBin::Add, y(), y())),
                       Box::new(IntBin(::syntax::IntBin::Sub, z(), x())));
        assert_eq!(fv(&expr), vec![y(), z()].into_iter().collect());
    }
    #[test]
    fn test_fv_let_unused() {
        use self::KNormal::*;
        // let x: int = y + y in z, fv(...) = {y, z}
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = || "z".to_string();
        let expr = Let((x(), Type::Int),
                       Box::new(IntBin(::syntax::IntBin::Add, y(), y())),
                       Box::new(Var(z())));
        assert_eq!(fv(&expr), vec![y(), z()].into_iter().collect());
    }
    #[test]
    fn test_g_if() {
        let mut id_gen = IdGen::new();
        let extenv = HashMap::new();
        // if x = x then y else z
        // ==> IfComp(Eq, x, x, y, z)
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = || "z".to_string();
        let env = vec![(x(), Type::Int),
                       (y(), Type::Int),
                       (z(), Type::Int)].into_iter().collect();
        let expr = Syntax::If(Box::new(Syntax::CompBin(CompBin::Eq, Box::new(Syntax::Var(x())), Box::new(Syntax::Var(x())))),
                              Box::new(Syntax::Var(y())),
                              Box::new(Syntax::Var(z())));
        assert_eq!(g(&env, expr, &mut id_gen, &extenv),
                   (KNormal::IfComp(CompBin::Eq,
                                    x(), x(),
                                    Box::new(KNormal::Var(y())),
                                    Box::new(KNormal::Var(z()))), Type::Int))
    }
    #[test]
    fn test_g_app() {
        let mut id_gen = IdGen::new();
        let extenv = HashMap::new();
        // x y z
        // ==> App(x, [y, z])
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = || "z".to_string();
        let env = vec![(x(), Type::Fun(Box::new([Type::Int, Type::Int]),
                                       Box::new(Type::Int))),
                       (y(), Type::Int),
                       (z(), Type::Int)].into_iter().collect();
        let expr = Syntax::App(Box::new(Syntax::Var(x())),
                               Box::new([Syntax::Var(y()), Syntax::Var(z())]));
        assert_eq!(g(&env, expr, &mut id_gen, &extenv),
                   (KNormal::App(x(), Box::new([y(), z()])),
                    Type::Int))
    }
    #[test]
    fn test_g_extfunapp() {
        let mut id_gen = IdGen::new();
        // x y z
        // ==> App(x, [y, z])
        let y = || "y".to_string();
        let print_int = || "print_int".to_string();
        let mut extenv = HashMap::new();
        extenv.insert(print_int(),
                      Type::Fun(Box::new([Type::Int]),
                      Box::new(Type::Unit)));
        let env = vec![(y(), Type::Int)].into_iter().collect();
        let expr = Syntax::App(Box::new(Syntax::Var(print_int())),
                               Box::new([Syntax::Var(y())]));
        assert_eq!(g(&env, expr, &mut id_gen, &extenv),
                   (KNormal::ExtFunApp(print_int(), Box::new([y()])),
                    Type::Unit))
    }
    #[test]
    fn test_g_tuple() {
        let mut id_gen = IdGen::new();
        let extenv = HashMap::new();
        // (x, y, 1.0)
        // ==> Let(d0, 1.0, Tuple(x, y, d0))
        let x = || "x".to_string();
        let y = || "y".to_string();
        let d0 = || "d0".to_string(); // Temporary variable of type float
        let env = vec![(x(), Type::Bool),
                       (y(), Type::Int)].into_iter().collect();
        let expr = Syntax::Tuple(Box::new([Syntax::Var(x()),
                                           Syntax::Var(y()),
                                           Syntax::Float(1.0.into())]));
        assert_eq!(g(&env, expr, &mut id_gen, &extenv),
                   (KNormal::Let((d0(), Type::Float),
                                 Box::new(KNormal::Float(1.0.into())),
                                 Box::new(KNormal::Tuple(
                                     Box::new([x(), y(), d0()])))),
                    Type::Tuple(Box::new(
                        [Type::Bool, Type::Int, Type::Float]))))
    }
    #[test]
    fn test_g_let_tuple() {
        let mut id_gen = IdGen::new();
        let extenv = HashMap::new();
        // let (x: bool, y: int, z: float) = f(1) in x
        // ==> LetTuple([x, y, z], f(1), x)
        // f: int -> (bool, int, float)
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = || "z".to_string();
        let f = || "f".to_string();
        let i0 = || "i0".to_string(); // temporary variable of type int
        let t1 = || "t1".to_string(); // temporary variable of type tuple
        let tuple_ty = Type::Tuple(
            Box::new([Type::Bool, Type::Int, Type::Float]));
        let env = vec![(f(),
                        Type::Fun(Box::new([Type::Int]),
                                  Box::new(tuple_ty.clone())))]
            .into_iter().collect();
        let args_list: Box<[(String, Type)]> = Box::new([(x(), Type::Bool),
                                  (y(), Type::Int),
                                  (z(), Type::Float)]);
        let expr = Syntax::LetTuple(
            args_list.clone(),
            Box::new(Syntax::App(Box::new(Syntax::Var(f())),
                                 Box::new([Syntax::Int(1)]))),
            Box::new(Syntax::Var(x())));
        let expected = (KNormal::Let((t1(), tuple_ty),
                                     Box::new(
                                         KNormal::Let(
                                             (i0(), Type::Int),
                                             Box::new(KNormal::Int(1)),
                                             Box::new(KNormal::App(
                                                 f(),
                                                 Box::new([i0()]))))),
                                     Box::new(
                                         KNormal::LetTuple(
                                             args_list,
                                             t1(),
                                             Box::new(KNormal::Var(x()))))),
        Type::Bool);
        assert_eq!(g(&env, expr, &mut id_gen, &extenv),
                   expected);
    }
    #[test]
    fn test_g_array_int() {
        let mut id_gen = IdGen::new();
        let extenv = HashMap::new();
        // create_array x 5
        // ==> Let((i0, Int), 5, ExtFunApp("create_array", [x, i0]))
        let x = || "x".to_string();
        let i0 = || "i0".to_string(); // Temporary variable of type int
        let env = vec![(x(), Type::Int)].into_iter().collect();
        let expr = Syntax::Array(Box::new(Syntax::Var(x())),
                                 Box::new(Syntax::Int(5)));
        let expected = KNormal::Let((i0(), Type::Int),
                                    Box::new(KNormal::Int(5)),
                                    Box::new(KNormal::ExtFunApp(
                                        "create_array".to_string(),
                                        Box::new([x(), i0()]))));
        assert_eq!(g(&env, expr, &mut id_gen, &extenv),
                   (expected,
                    Type::Array(Box::new(Type::Int))))
    }
    #[test]
    fn test_g_letrec() {
        let mut id_gen = IdGen::new();
        let extenv = HashMap::new();
        // let rec f x = g x (f y) in f z
        // ==> LetRec(KFundef { name: ("f", Fun([Int], Int)), args: [("x", Int)], body: Let(("i0", Int), App("f", ["y"]), App("g", ["x", "i0"])) }, App("f", ["z"]))
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = || "z".to_string();
        let f = || "f".to_string();
        let g_id = || "g".to_string();
        let i0 = || "i0".to_string(); // Temporary variable of type int
        let env = vec![(y(), Type::Int),
                       (z(), Type::Int),
                       (g_id(), Type::Fun(Box::new([Type::Int, Type::Int]),
                                          Box::new(Type::Int)))]
            .into_iter().collect();
        let body = Syntax::App(
            Box::new(Syntax::Var(g_id())),
            Box::new([Syntax::Var(x()), Syntax::App(
                Box::new(Syntax::Var(f())),
                Box::new([Syntax::Var(y())]))]));
        let fundef = Fundef {
            name: (f(), Type::Fun(Box::new([Type::Int]),
                                  Box::new(Type::Int))),
            args: Box::new([(x(), Type::Int)]),
            body: Box::new(body),
            
        };
        let expr = Syntax::LetRec(fundef,
                                  Box::new(Syntax::App(
                                      Box::new(Syntax::Var(f())),
                                      Box::new([Syntax::Var(z())])
                                  )));
        let body_trans = KNormal::Let(
            (i0(), Type::Int),
            Box::new(KNormal::App(f(), Box::new([y()]))),
            Box::new(KNormal::App(g_id(), Box::new([x(), i0()]))));
        let expected = (KNormal::LetRec(
            KFundef {
                name: (f(), Type::Fun(Box::new([Type::Int]),
                                      Box::new(Type::Int))),
                args: Box::new([(x(), Type::Int)]),
                body: Box::new(body_trans) },
            Box::new(KNormal::App(f(), Box::new([z()])))),
                        Type::Int);
        assert_eq!(g(&env, expr, &mut id_gen, &extenv), expected)
    }
}
