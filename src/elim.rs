extern crate std;

use k_normal::{KNormal, KFundef};
use k_normal::fv;

fn has_effect(e: &KNormal) -> bool {
    use self::KNormal::*;
    match e {
        Let(_, e1, e2) | IfComp(_, _, _, e1, e2) =>
            has_effect(e1) || has_effect(e2),
        LetRec(_, e) | LetTuple(_, _, e) => has_effect(e),
        App(_, _) | Put(_, _, _) | ExtFunApp(_, _) => true,
        _ => false,
    }
}

// O(|e|^2) in a worst-case scenario, because fv is called many times
pub fn f(e: KNormal) -> KNormal {
    use self::KNormal::*;
    macro_rules! invoke {
        ($e: expr) => { Box::new(f(*$e)) }
    }
    match e {
        IfComp(op, x, y, e1, e2) =>
            IfComp(op, x, y, invoke!(e1), invoke!(e2)),
        Let((x, t), e1, e2) => {
            let e1p = invoke!(e1);
            let e2p = invoke!(e2);
            if has_effect(&e1p) || fv(&e2p).contains(&x) {
                Let((x, t), e1p, e2p)
            } else {
                println!("eliminating variable {}.", x);
                *e2p
            }
        },
        LetRec(KFundef { name: (x, t), args: yts, body: e1 }, e2) => {
            let e2p = invoke!(e2);
            if fv(&e2p).contains(&x) {
                LetRec(KFundef { name: (x, t), args: yts, body: invoke!(e1) },
                       e2p)
            } else {
                println!("eliminating function {}.", x);
                *e2p
            }
        },
        LetTuple(xts, y, e) => {
            let ep = invoke!(e);
            let live = fv(&ep);
            let any_used = xts.iter().any(|(x, _)| live.contains(x));
            if any_used {
                LetTuple(xts, y, ep)
            } else {
                println!("eliminating variables {:?}.", xts);
                *ep
            }
        },
        e => e,
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_unnecessary_let() {
        use k_normal::KNormal::*;
        use syntax::Type;
        use super::f;
        // let x = 1 in let y = x in 1 should become
        // 1
        let x = || "x".to_string();
        let y = || "y".to_string();
        let e = Let((x(), Type::Int), Box::new(Int(1)),
                    Box::new(Let((y(), Type::Int), Box::new(Var(x())),
                                 Box::new(Int(1)))));
        let e2 = Int(1);
        assert_eq!(f(e), e2);
    }
    #[test]
    fn test_unnecessary_letrec() {
        use k_normal::KFundef;
        use k_normal::KNormal::*;
        use syntax::{Type, IntBin};
        use super::f;
        // let rec f x = x + x in 6 should become
        // 6
        let x = || "x".to_string();
        let ff = || "f".to_string();
        let e = LetRec(KFundef {
            name: (ff(), Type::Fun(Box::new([Type::Int]), Box::new(Type::Int))),
            args: Box::new([(x(), Type::Int)]),
            body: Box::new(IntBin(IntBin::Add, x(), x())) },
                       Box::new(Int(6)));
        let e2 = Int(6);
        assert_eq!(f(e), e2);
    }
    #[test]
    fn test_unnecessary_lettuple() {
        use k_normal::KNormal::*;
        use syntax::Type;
        use super::f;
        // let (x, y) = z in 6 should become
        // 6
        let e = LetTuple(
            Box::new([("x".to_string(), Type::Int),
                      ("y".to_string(), Type::Int)]),
            "z".to_string(),
            Box::new(Int(6)));
        let e2 = Int(6);
        assert_eq!(f(e), e2);
    }
}
