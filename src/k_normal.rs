use ordered_float::OrderedFloat;
use syntax::{IntBin, FloatBin, CompBin, Type};
use std::collections::HashSet;

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

pub struct KFundef {
    pub name: (String, Type),
    pub args: Box<[(String, Type)]>,
    pub body: Box<KNormal>,
}

/*
 * Free variables in an AST
 */
fn fv(e: &KNormal) -> HashSet<String> {
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
        KNormal::Unit => HashSet::new(),
        KNormal::Int(_) => HashSet::new(),
        KNormal::Float(_) => HashSet::new(),
        KNormal::ExtArray(_) => HashSet::new(),
        KNormal::Neg(ref x) => build_set!(x),
        KNormal::FNeg(ref x) => build_set!(x),
        KNormal::IntBin(_, ref x, ref y) => build_set!(x, y),
        KNormal::FloatBin(_, ref x, ref y) => build_set!(x, y),
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
        KNormal::Get(ref x, ref y) => build_set!(x, y),
        KNormal::Put(ref x, ref y, ref z) => build_set!(x, y, z),
        KNormal::ExtFunApp(_, ref xs) => xs.iter().cloned().collect(),
    }
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
}
