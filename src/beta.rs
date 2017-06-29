extern crate std;

use k_normal::{KNormal, KFundef};
use std::collections::HashMap;


fn g(env: &HashMap<String, String>, e: KNormal) -> KNormal {
    use self::KNormal::*;
    macro_rules! invoke {
        ($e: expr) => { Box::new(g(env, *$e)) }
    }
    macro_rules! find {
        ($name: expr) => {
            match env.get(&$name) {
                Some(p) => p.clone(),
                None => $name,
            }
        }
    }
    macro_rules! find_vec_mut {
        ($vec: expr) => {
            for v in $vec.iter_mut() {
                *v = find!(std::mem::replace(v, "".to_string()));
            }
        }
    }
    match e {
        Unit => Unit,
        Int(i) => Int(i),
        Float(f) => Float(f),
        Neg(x) => Neg(find!(x)),
        IntBin(op, x, y) => IntBin(op, find!(x), find!(y)),
        FNeg(x) => FNeg(find!(x)),
        FloatBin(op, x, y) => FloatBin(op, find!(x), find!(y)),
        IfComp(op, x, y, e1, e2) =>
            IfComp(op, find!(x), find!(y), invoke!(e1), invoke!(e2)),
        Let((x, t), e1, e2) => {
            match g(env, *e1) {
                Var(y) => {
                    println!("beta-reducing {} = {}.", x, y);
                    let mut cp_env = env.clone();
                    cp_env.insert(x, y);
                    g(&cp_env, *e2)
                },
                e1p => {
                    Let((x, t), Box::new(e1p), invoke!(e2))
                }
            }
        },
        Var(x) => Var(find!(x)),
        LetRec(KFundef { name: xt, args: mut yts, body: e1 }, e2) => {
            for i in 0 .. yts.len() {
                let entry = std::mem::replace(&mut yts[i].0, "".to_string());
                yts[i].0 = find!(entry);
            }
            LetRec(KFundef { name: xt, args: yts,
                            body: invoke!(e1) },
                   invoke!(e2))
        },
        App(x, mut ys) => {
            find_vec_mut!(ys);
            App(find!(x), ys)
        },
        Tuple(mut xs) => {
            find_vec_mut!(xs);
            Tuple(xs)
        },
        LetTuple(xts, y, e) => LetTuple(xts, find!(y), invoke!(e)),
        Get(x, y) => Get(find!(x), find!(y)),
        Put(x, y, z) => Put(find!(x), find!(y), find!(z)),
        ExtArray(x) => ExtArray(x),
        ExtFunApp(x, mut ys) => {
            find_vec_mut!(ys);
            ExtFunApp(x, ys)
        }
    }
}

pub fn f(e: KNormal) -> KNormal {
    g(&HashMap::new(), e)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_let_var() {
        use k_normal::KNormal::*;
        use syntax::Type;
        use super::f;
        // let x = 1 in let y = x in y should be
        // let x = 1 in x
        let x = || "x".to_string();
        let y = || "y".to_string();
        let e = Let((x(), Type::Int), Box::new(Int(1)),
                    Box::new(Let((y(), Type::Int), Box::new(Var(x())),
                                 Box::new(Var(y())))));
        let e2 = Let((x(), Type::Int), Box::new(Int(1)),
                     Box::new(Var(x())));
        assert_eq!(f(e), e2);
    }
    #[test]
    fn test_does_not_affect() {
        use k_normal::KNormal::*;
        use syntax::Type;
        use super::f;
        let x = || "x".to_string();
        let y = || "y".to_string();
        let e = Let((x(), Type::Int), Box::new(Int(1)),
                    Box::new(Let((y(), Type::Int), Box::new(Neg(x())),
                                 Box::new(Var(y())))));
        assert_eq!(f(e.clone()), e);
    }
}
