use std::collections::HashMap;
use alpha;
use k_normal::{KNormal, KFundef};
use syntax::Type;
use id::IdGen;

fn size(e: &KNormal) -> usize {
    use self::KNormal::*;
    match *e {
        IfComp(_, _, _, ref e1, ref e2) |
        Let(_, ref e1, ref e2) |
        LetRec(KFundef { name: _, args: _, body: ref e1 }, ref e2) =>
            1 + size(e1) + size(e2),
        LetTuple(_, _, ref e) => 1 + size(e),
        _ => 1,
    }
}


fn g(env: &HashMap<String, (Box<[(String, Type)]>, KNormal)>, e: KNormal, id_gen: &mut IdGen,
     inline_threshold: usize)
     -> KNormal {
    use self::KNormal::*;
    macro_rules! invoke {
        ($e: expr) => { Box::new(g(env, *$e, id_gen, inline_threshold)) }
    }
    match e {
        IfComp(op, x, y, e1, e2) =>
            IfComp(op, x, y, invoke!(e1), invoke!(e2)),
        Let((x, t), e1, e2) =>
            Let((x, t), invoke!(e1), invoke!(e2)),
        LetRec(KFundef { name: (x, t), args: yts, body: e1 }, e2) => {
            let mut env = env.clone();
            if size(&e1) <= inline_threshold {
                env.insert(x.clone(), (yts.clone(), (&e1 as &KNormal).clone()));
            }
            LetRec(KFundef {
                name: (x, t), args: yts,
                body: Box::new(g(&env, *e1, id_gen, inline_threshold)) },
                   Box::new(g(&env, *e2, id_gen, inline_threshold)))
        },
        App(x, ys) => {
            if let Some(zse) = env.get(&x) {
                let (zs, e) = zse.clone();
                eprintln!("inlining {}.", x);
                let mut alpha_env = HashMap::new();
                assert_eq!(ys.len(), zs.len());
                for ((z, _t), y) in zs.into_vec().into_iter().zip(ys.into_vec().into_iter()) {
                    alpha_env.insert(z, y);
                }
                alpha::g(&alpha_env, e, id_gen)
            } else {
                App(x, ys)
            }
        },
        LetTuple(xts, y, e) =>
            LetTuple(xts, y, invoke!(e)),
        e => e,
    }
}

pub fn f(e: KNormal, id_gen: &mut IdGen, inline_threshold: usize) -> KNormal {
    g(&HashMap::new(), e, id_gen, inline_threshold)
}


#[cfg(test)]
mod tests {
    #[test]
    fn test_inline() {
        use id::IdGen;
        use k_normal::KFundef;
        use k_normal::KNormal::{LetRec, App, IntBin};
        use syntax::Type;
        use super::f;
        // let rec f x = x + x in f y ===> y + y
        let ff = || "f".to_string();
        let x = || "x".to_string();
        let y = || "y".to_string();
        let e = LetRec(
            KFundef {
                name: (ff(), Type::Fun(
                    Box::new([Type::Int]), Box::new(Type::Int))),
                args: Box::new([("x".to_string(), Type::Int)]),
                body: Box::new(IntBin(
                    ::syntax::IntBin::Add,
                    x(), x())) },
            Box::new(App(ff(), Box::new([y()]))));
        let e_expected = LetRec(
            KFundef {
                name: (ff(), Type::Fun(
                    Box::new([Type::Int]), Box::new(Type::Int))),
                args: Box::new([("x".to_string(), Type::Int)]),
                body: Box::new(IntBin(
                    ::syntax::IntBin::Add,
                    x(), x())) },
            Box::new(IntBin(
                ::syntax::IntBin::Add,
                y(), y())));
        let mut id_gen = IdGen::new();
        assert_eq!(f(e, &mut id_gen, 5000), e_expected);
    }
}
