extern crate std;

use k_normal::{KNormal, KFundef};
use syntax::{IntBin, FloatBin, CompBin};
use std::collections::HashMap;

// Changed from the original ml source
enum Const {
    IntConst(i64),
    FloatConst(f64),
    TupleConst(Box<[Const]>),
}

type ConstEnv = HashMap<String, Const>;

fn add_env(env: &mut ConstEnv, x: String, e: &KNormal) {
    use self::KNormal::*;
    use self::Const::*;
    match *e {
        Int(v) => { env.insert(x, IntConst(v)); },
        Float(f) => { env.insert(x, FloatConst(f.into())); },
        _ => (),
    }
}


fn findi(x: &str, env: &ConstEnv) -> Option<i64> {
    use self::Const::*;
    match env.get(x) {
        Some(&IntConst(v)) => Some(v),
        _ => None,
    }
}
fn findf(x: &str, env: &ConstEnv) -> Option<f64> {
    use self::Const::*;
    match env.get(x) {
        Some(&FloatConst(v)) => Some(v.into()),
        _ => None,
    }
}

/// env is mutable, which was not in the original ml source.
/// Thanks to alpha conversion, this is ok.
fn g(env: &mut ConstEnv, e: KNormal) -> KNormal {
    use self::KNormal::*;
    use self::IntBin::*;
    use self::FloatBin::*;
    use self::CompBin::*;
    macro_rules! invoke {
        ($e: expr) => { Box::new(g(env, *$e)) }
    }
    match e {
        Unit => Unit,
        Int(i) => Int(i),
        Float(f) => Float(f),
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
        LetTuple(xts, y, e) => // TODO const_fold for tuple
            LetTuple(xts, y, invoke!(e)),
        e => e,
    }
}

pub fn f(e: KNormal) -> KNormal {
    g(&mut HashMap::new(), e)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_constfold_ops() {
        use k_normal::KNormal::*;
        use syntax::{Type, IntBin};
        use super::f;
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
        assert_eq!(f(e), e2);
    }
}
