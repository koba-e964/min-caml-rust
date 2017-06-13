extern crate std;

use syntax::Type;
use k_normal::{KNormal, KFundef};

fn insert(e: KNormal, xt: (String, Type), e2: KNormal) -> KNormal {
    use self::KNormal::*;
    macro_rules! invoke {
        ($e: expr) => (Box::new(insert(*$e, xt, e2)));
    }
    match e {
        Let(yt, e3, e4) => Let(yt, e3, invoke!(e4)),
        LetRec(fundefs, e) => LetRec(fundefs, invoke!(e)),
        LetTuple(yts, z, e) => LetTuple(yts, z, invoke!(e)),
        e => Let(xt, Box::new(e), Box::new(f(e2))),
    }
}

/// For example, let x = let y = 1 in y + 1 in x + 3
/// becomes
/// let y = 1 in let x = y + 1 in x + 3

pub fn f(e: KNormal) -> KNormal {
    use self::KNormal::*;
    macro_rules! invoke {
        ($e:expr) => (Box::new(f(*$e)));
    }
    match e {
        IfComp(op, x, y, e1, e2) => IfComp(op, x, y, invoke!(e1), invoke!(e2)),
        LetRec(KFundef { name: xt, args: yts, body: e1 }, e2) =>
            LetRec(KFundef { name: xt, args: yts,
                            body: invoke!(e1) },
                   invoke!(e2)),
        Let(xt, e1, e2) =>
            insert(f(*e1), xt, *e2),
        LetTuple(xts, y, e) => LetTuple(xts, y, invoke!(e)),
        e => e,
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_let_var() {
        use k_normal::KNormal::*;
        use syntax::Type;
        use syntax::IntBin::Add; 
        use super::f;
        // let x = let y = 1 in y + k in x + l
        // should become
        // let y = 1 in let x = y + k in x + l
        let x = || "x".to_string();
        let y = || "y".to_string();
        let k = || "k".to_string();
        let l = || "l".to_string();
        let e = Let((x(), Type::Int),
                    Box::new(Let((y(), Type::Int), Box::new(Int(1)),
                                 Box::new(IntBin(Add, y(), k())))),
                    Box::new(IntBin(Add, x(), l())));
        let e2 = Let((y(), Type::Int), Box::new(Int(1)),
                     Box::new(Let((x(), Type::Int),
                                  Box::new(IntBin(Add, y(), k())),
                                  Box::new(IntBin(Add, x(), l())))));
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
