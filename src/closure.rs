use ordered_float::OrderedFloat;
use syntax::{IntBin, FloatBin, CompBin, Type};
use k_normal::{KNormal, KFundef};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cls {
    pub entry: String,
    pub actual_fv: Box<[String]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Closure {
    Unit,
    Int(i64),
    Float(OrderedFloat<f64>),
    Neg(String),
    IntBin(IntBin, String, String),
    FNeg(String),
    FloatBin(FloatBin, String, String),
    IfComp(CompBin, String, String, Box<Closure>, Box<Closure>),
    Let((String, Type), Box<Closure>, Box<Closure>),
    Var(String),
    MakeCls(String, Type, Cls, Box<Closure>),
    AppCls(String, Box<[String]>),
    AppDir(String, Box<[String]>),
    Tuple(Box<[String]>),
    LetTuple(Box<[(String, Type)]>, String, Box<Closure>),
    Get(String, String),
    Put(String, String, String),
    ExtArray(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fundef {
    pub name: (String, Type),
    pub args: Box<[(String, Type)]>,
    pub formal_fv: Box<[(String, Type)]>,
    pub body: Box<Closure>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prog(Box<[Fundef]>, Closure);

macro_rules! build_set {
    ($($x:expr),*) => ({
        let mut h = HashSet::new();
        $(h.insert($x.clone());)*
            h
    })
}
fn fv(e: &Closure) -> HashSet<String> {
    use self::Closure::*;
    macro_rules! invoke {
        ($e:expr) => (fv($e));
    }
    match *e {
        Unit | Int(_) | Float(_) | ExtArray(_) => HashSet::new(),
        Neg(ref x) | FNeg(ref x) => build_set!(x),
        IntBin(_, ref x, ref y) | FloatBin(_, ref x, ref y) |
        Get(ref x, ref y) =>
            build_set!(x, y),
        IfComp(_, ref x, ref y, ref e1, ref e2) => {
            let h = build_set!(x, y);
            let s1 = invoke!(e1);
            let s2 = invoke!(e2);
            &(&h | &s1) | &s2
        },
        Let((ref x, _), ref e1, ref e2) => {
            let s1 = invoke!(e1);
            let s2 = &invoke!(e2) - &build_set!(x);
            &s1 | &s2
        }
        Var(ref x) => build_set!(x),
        MakeCls(ref x, _, Cls { entry: _, actual_fv: ref ys }, ref e) =>
            &(&ys.iter().cloned().collect() | &invoke!(e)) - &build_set!(x),
        AppCls(ref x, ref ys) =>
            &build_set!(x) | &ys.iter().cloned().collect::<HashSet<_>>(),
        AppDir(_, ref xs) | Tuple(ref xs) => xs.iter().cloned().collect(),
        LetTuple(ref xs, ref y, ref e) => {
            let tmp: HashSet<String> = xs.iter().map(|x| x.0.clone())
                .collect(); // S.of_list (List.map fst xs)
            &build_set!(y) | &(&invoke!(e) - &tmp)
        },
        Put(ref x, ref y, ref z) => build_set!(x, y, z),
    }
}

fn g(env: &HashMap<String, Type>, known: &HashSet<String>,
     e: KNormal, toplevel: &mut Vec<Fundef>) -> Closure {
    use self::Closure::*;
    macro_rules! invoke {
        ($e: expr) => { Box::new(g(env, known, *$e, toplevel)) }
    }
    match e {
        KNormal::Unit => Unit,
        KNormal::Int(i) => Int(i),
        KNormal::Float(f) => Float(f),
        KNormal::Neg(x) => Neg(x),
        KNormal::IntBin(op, x, y) => IntBin(op, x, y),
        KNormal::FNeg(x) => FNeg(x),
        KNormal::FloatBin(op, x, y) => FloatBin(op, x, y),
        KNormal::IfComp(op, x, y, e1, e2) =>
            IfComp(op, x, y, invoke!(e1), invoke!(e2)),
        KNormal::Let((x, t), e1, e2) => {
            let mut cp_env = env.clone();
            cp_env.insert(x.clone(), t.clone());
            Let((x, t), invoke!(e1), Box::new(g(&cp_env, known, *e2, toplevel)))
        },
        KNormal::Var(x) => Var(x),
        KNormal::LetRec(KFundef { name: (x, t), args: yts, body: e1}, e2) => {
            /* Follow the original code */
            let mut toplevel_cp = toplevel.clone();
            let mut env_p = env.clone();
            env_p.insert(x.clone(), t.clone());
            let mut known_p = known.clone();
            known_p.insert(x.clone());
            let mut env_p2 = env_p.clone();
            for &(ref y, ref t) in yts.iter() {
                env_p2.insert(y.clone(), t.clone());
            }
            let e1p = g(&env_p2, &known_p, (*e1).clone(), &mut toplevel_cp);
            /* Check if e1p contains free variables */
            let zs =
                &fv(&e1p) - &yts.iter().map(|&(ref y, _)| y.clone()).collect();
            let (known_p, e1p) = if zs.is_empty() {
                *toplevel = toplevel_cp;
                (&known_p, e1p)
            } else {
                eprintln!("free variables(s) {:?} found in function {}",
                          zs, x);
                eprintln!("function {} cannot be directly applied in fact.", x);
                let e1p = g(&env_p2, known, *e1, toplevel);
                (known, e1p)
            };
            let zs: Vec<String> = (&zs - &build_set!(x)).into_iter().collect();
            let zts: Vec<(String, Type)> = zs.iter().map(|&ref z| (z.clone(), env.get(z).unwrap().clone())).collect();
            toplevel.push(Fundef { name: (x.clone(), t.clone()),
                                   args: yts,
                                   formal_fv: zts.into_boxed_slice(),
                                   body: Box::new(e1p) });
            let e2p = g(&env_p, known_p, *e2, toplevel);
            if fv(&e2p).contains(&x) {
                MakeCls(x.clone(), t,
                        Cls { entry: x, actual_fv: zs.into_boxed_slice() },
                        Box::new(e2p))
            } else {
                eprintln!("eliminating closure {}", x);
                e2p
            }
        },
        KNormal::App(x, ys) => {
            if known.contains(&x) {
                AppDir(x, ys)
            } else {
                AppCls(x, ys)
            }
        },
        KNormal::Tuple(xs) => Tuple(xs),
        KNormal::LetTuple(xts, y, e) => {
            let mut cp_env = env.clone();
            for &(ref x, ref t) in xts.iter() {
                cp_env.insert(x.clone(), t.clone());
            }
            LetTuple(xts, y, Box::new(g(&cp_env, known, *e, toplevel)))
        },
        KNormal::Get(x, y) => Get(x, y),
        KNormal::Put(x, y, z) => Put(x, y, z),
        KNormal::ExtArray(x) => ExtArray(x),
        KNormal::ExtFunApp(x, ys) => AppDir(format!("min_caml_{}", x), ys),
    }
}


pub fn f(e: KNormal) -> Prog {
    let mut toplevel = Vec::new();
    let e = g(&HashMap::new(), &HashSet::new(), e, &mut toplevel);
    Prog(toplevel.into_boxed_slice(), e)
}
