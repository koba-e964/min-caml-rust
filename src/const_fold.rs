extern crate std;

use k_normal::{KNormal, KFundef};
use syntax::{IntBin, FloatBin, CompBin, Type};
use std::collections::HashMap;
use id::IdGen;

// Changed from the original ml source
#[derive(Clone, Debug)]
enum Const {
    IntConst(i64),
    FloatConst(f64),
    TupleConst(Box<[Const]>),
}

type ConstEnv = HashMap<String, Const>;

fn add_env(env: &mut ConstEnv, x: String, e: &KNormal) {
    use self::KNormal::*;
    use self::Const::*;
    match e {
        Int(v) => { env.insert(x, IntConst(*v)); },
        Float(f) => { env.insert(x, FloatConst((*f).into())); },
        // Tuple const folding is done only if all components are constant.
        Tuple(xs) => {
            let result: Option<Vec<Const>> =
                xs.iter().map(|x| env.get(x).cloned()).collect();
            if let Some(result) = result {
                env.insert(x, TupleConst(result.into_boxed_slice()));
            }
        },
        _ => (),
    }
}


fn findi(x: &str, env: &ConstEnv) -> Option<i64> {
    use self::Const::*;
    match env.get(x) {
        Some(IntConst(v)) => Some(*v),
        _ => None,
    }
}
fn findf(x: &str, env: &ConstEnv) -> Option<f64> {
    use self::Const::*;
    match env.get(x) {
        Some(FloatConst(v)) => Some(*v),
        _ => None,
    }
}
fn findt(x: &str, env: &ConstEnv) -> Option<Box<[Const]>> {
    use self::Const::*;
    match env.get(x) {
        Some(TupleConst(vals)) => Some(vals.clone()),
        _ => None,
    }
}

fn value_to_expr(x: Const, id_gen: &mut IdGen) -> (KNormal, Type) {
    use self::Const::*;
    use self::KNormal::*;
    match x {
        IntConst(v) => (Int(v), Type::Int),
        FloatConst(f) => (Float(f.into()), Type::Float),
        TupleConst(v) => {
            let mut tvar = Vec::new();
            let mut expr = Vec::new();
            let mut tys = Vec::new();
            for elem in v.iter() {
                let (e, ty) = value_to_expr(elem.clone(), id_gen);
                let tmp = id_gen.gen_tmp(&ty);
                tvar.push(tmp);
                expr.push(e);
                tys.push(ty);
            }
            let mut ret = Tuple(tvar.clone().into_boxed_slice());
            for i in (0 .. v.len()).rev() {
                ret = Let((tvar[i].clone(), tys[i].clone()),
                          Box::new(expr[i].clone()), Box::new(ret));
            }
            (ret, Type::Tuple(tys.into_boxed_slice()))
        }
    }
}

/// env is mutable, which was not in the original ml source.
/// Thanks to alpha conversion, this is ok.
fn g(env: &mut ConstEnv, e: KNormal, id_gen: &mut IdGen) -> KNormal {
    use self::KNormal::*;
    use self::IntBin::*;
    use self::FloatBin::*;
    use self::CompBin::*;
    macro_rules! invoke {
        ($e: expr) => { Box::new(g(env, *$e, id_gen)) }
    }
    match e {
        Unit | Int(_) | Float(_) => e,
        Neg(x) =>
            match findi(&x, env) {
                Some(v) => Int(-v),
                None => Neg(x),
            },
        IntBin(op, x, y) =>
            match (findi(&x, env), findi(&y, env)) {
                (Some(v1), Some(v2)) => Int(match op {
                    Add => v1 + v2,
                    Sub => v1 - v2,
                }),
                _ => IntBin(op, x, y),
            },
        FNeg(x) => {
            match findf(&x, env) {
                Some(f) => Float((-f).into()),
                None => FNeg(x),
            }
        },
        FloatBin(op, x, y) =>
            match (findf(&x, env), findf(&y, env)) {
                (Some(f1), Some(f2)) => Float((match op {
                    FAdd => f1 + f2,
                    FSub => f1 - f2,
                    FMul => f1 * f2,
                    FDiv => f1 / f2,
                }).into()),
                _ => FloatBin(op, x, y),
            },
        IfComp(op, x, y, e1, e2) =>
            match (findi(&x, env), findi(&y, env)) {
                (Some(v1), Some(v2)) => {
                    let res = match op {
                        Eq => v1 == v2,
                        LE => v1 <= v2,
                    };
                    if res {
                        *e1
                    } else {
                        *e2
                    }
                },
                _ => IfComp(op, x, y, e1, e2),
            },
        Let((x, t), e1, e2) => {
            let e1p = invoke!(e1);
            add_env(env, x.clone(), &e1p);
            let e2p = invoke!(e2);
            Let((x, t), e1p, e2p)
        },
        Var(x) =>
            match findi(&x, env) {
                Some(v) => Int(v),
                None => Var(x),
            },
        LetRec(KFundef { name: (x, t), args: yts, body: e1 }, e2) =>
            LetRec(KFundef { name: (x, t), args: yts, body: invoke!(e1) },
                   invoke!(e2)),
        LetTuple(xts, y, e) =>
            match findt(&y, env) {
                Some(vals) => {
                    if xts.len() != vals.len() {
                        panic!("Something is wrong... |xts| != |vals|");
                    }
                    for i in 0 .. xts.len() {
                        env.insert(xts[i].0.clone(), vals[i].clone());
                    }
                    let mut e = *e;
                    for i in (0 .. xts.len()).rev() {
                        let v = vals[i].clone();
                        e = Let(xts[i].clone(),
                                Box::new(value_to_expr(v, id_gen).0), Box::new(e));
                    }
                    g(env, e, id_gen)
                },
                None => LetTuple(xts, y, invoke!(e)),
            },
        e => e,
    }
}

pub fn f(e: KNormal, id_gen: &mut IdGen) -> KNormal {
    g(&mut HashMap::new(), e, id_gen)
}

#[cfg(test)]
mod tests {
    use id::IdGen;
    #[test]
    fn test_constfold_ops() {
        use k_normal::KNormal::*;
        use syntax::{Type, IntBin};
        use super::f;
        let mut id_gen = IdGen::new();
        // let x = 1 in let y = x + x in let z = y - x in z should become
        // 1
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = || "z".to_string();
        let e = Let((x(), Type::Int), Box::new(Int(1)),
                    Box::new(Let((y(), Type::Int),
                                 Box::new(IntBin(IntBin::Add, x(), x())),
                                 Box::new(Let((z(), Type::Int),
                                              Box::new(IntBin(IntBin::Sub, y(), x())),
                                              Box::new(Var(z()))))
                    )));
        let e2 = Let((x(), Type::Int), Box::new(Int(1)),
                    Box::new(Let((y(), Type::Int),
                                 Box::new(Int(2)),
                                 Box::new(Let((z(), Type::Int),
                                              Box::new(Int(1)),
                                              Box::new(Int(1))))
                    )));
        assert_eq!(f(e, &mut id_gen), e2);
    }
    #[test]
    fn test_constfold_tuple() {
        use k_normal::KNormal::*;
        use syntax::{Type, IntBin};
        use super::f;
        let mut id_gen = IdGen::new();
        // let x = 1 in let y = 2 in let z = (x, y) in let (u, v) = z in u + v
        // should become
        // let x = 1 in let y = 2 in let z = (x, y) in let u = 1 in
        // let v = 2 in 3
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = || "z".to_string();
        let u = || "u".to_string();
        let v = || "v".to_string();
        let internal = LetTuple(Box::new([(u(), Type::Int), (v(), Type::Int)]),
                                z(), Box::new(IntBin(IntBin::Add, u(), v())));
        let e = Let((x(), Type::Int), Box::new(Int(1)),
                    Box::new(Let((y(), Type::Int),
                                 Box::new(Int(2)),
                                 Box::new(Let((z(), Type::Tuple(Box::new([Type::Int, Type::Int]))),
                                              Box::new(Tuple(Box::new([x(), y()]))),
                                              Box::new(internal)))
                    )));
        let internal2 = Let((u(), Type::Int), Box::new(Int(1)),
                            Box::new(Let((v(), Type::Int), Box::new(Int(2)),
                                         Box::new(Int(3)))));
        let e2 = Let((x(), Type::Int), Box::new(Int(1)),
                    Box::new(Let((y(), Type::Int),
                                 Box::new(Int(2)),
                                 Box::new(Let((z(), Type::Tuple(Box::new([Type::Int, Type::Int]))),
                                              Box::new(Tuple(Box::new([x(), y()]))),
                                              Box::new(internal2)))
                    )));
        assert_eq!(f(e, &mut id_gen), e2);
    }
}
