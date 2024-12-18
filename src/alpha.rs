use crate::id::IdGen;
use crate::k_normal::{KFundef, KNormal};
use std::collections::HashMap;

pub fn g(env: &HashMap<String, String>, e: KNormal, id_gen: &mut IdGen) -> KNormal {
    use self::KNormal::*;
    macro_rules! invoke {
        ($e: expr) => {
            Box::new(g(env, *$e, id_gen))
        };
    }
    let find = |name: String| match env.get(&name) {
        Some(p) => p.clone(),
        None => name,
    };
    let find_vec_mut = |vec: &mut [String]| {
        for v in vec.iter_mut() {
            *v = find(std::mem::take(v));
        }
    };
    match e {
        Unit => Unit,
        Int(i) => Int(i),
        Float(f) => Float(f),
        Neg(x) => Neg(find(x)),
        IntBin(op, x, y) => IntBin(op, find(x), find(y)),
        FNeg(x) => FNeg(find(x)),
        FloatBin(op, x, y) => FloatBin(op, find(x), find(y)),
        IfComp(op, x, y, e1, e2) => IfComp(op, find(x), find(y), invoke!(e1), invoke!(e2)),
        Let((x, t), e1, e2) => {
            let xn = id_gen.gen_id(&format!("{}_", x));
            let mut cp_env = env.clone();
            cp_env.insert(x, xn.clone());
            Let((xn, t), invoke!(e1), Box::new(g(&cp_env, *e2, id_gen)))
        }
        Var(x) => Var(find(x)),
        LetRec(
            KFundef {
                name: (x, t),
                args: mut yts,
                body: e1,
            },
            e2,
        ) => {
            let mut cp_env = env.clone();
            let xname = id_gen.gen_id(&x);
            cp_env.insert(x, xname.clone());
            let mut cp_env_body = cp_env.clone();
            for item in yts.iter_mut() {
                let new_name = id_gen.gen_id(&item.0);
                cp_env_body.insert(item.0.clone(), new_name.clone());
                item.0 = new_name;
            }
            LetRec(
                KFundef {
                    name: (xname, t),
                    args: yts,
                    body: Box::new(g(&cp_env_body, *e1, id_gen)),
                },
                Box::new(g(&cp_env, *e2, id_gen)),
            )
        }
        App(x, mut ys) => {
            find_vec_mut(&mut ys);
            App(find(x), ys)
        }
        Tuple(mut xs) => {
            find_vec_mut(&mut xs);
            Tuple(xs)
        }
        LetTuple(mut xts, y, e) => {
            let mut cp_env = env.clone();
            for item in xts.iter_mut() {
                let entry = std::mem::replace(&mut item.0, "".to_string());
                let newx = id_gen.gen_id(&entry);
                item.0 = newx.clone();
                cp_env.insert(entry, newx);
            }
            LetTuple(xts, find(y), Box::new(g(&cp_env, *e, id_gen)))
        }
        Get(x, y) => Get(find(x), find(y)),
        Put(x, y, z) => Put(find(x), find(y), find(z)),
        ExtArray(x) => ExtArray(x),
        ExtFunApp(x, mut ys) => {
            find_vec_mut(&mut ys);
            ExtFunApp(x, ys)
        }
    }
}

pub fn f(e: KNormal, id_gen: &mut IdGen) -> KNormal {
    g(&HashMap::new(), e, id_gen)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_extarray() {
        use super::f;
        use crate::id::IdGen;
        use crate::k_normal::KNormal::ExtArray;
        let x = || "x".to_string();
        let e = ExtArray(x());
        let mut id_gen = IdGen::new();
        assert_eq!(f(e, &mut id_gen), ExtArray(x()));
    }
}
