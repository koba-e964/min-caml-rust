use std::collections::HashMap;
use syntax;
use syntax::Type;
use id;
use id::IdGen;
use closure;
use closure::Closure;
use x86::asm::{Asm, CompBin, Exp, FCompBin, IdOrImm, Fundef, Prog, concat};
use ordered_float::OrderedFloat;

fn g(data: &mut Vec<(id::L, OrderedFloat<f64>)>, env: &HashMap<String, Type>,
     e: Closure, id_gen: &mut IdGen) -> Asm {
    use self::Asm::*;
    use self::Exp::*;
    macro_rules! invoke {
        ($e:expr) => ({
            Box::new(g(data, env, *$e, id_gen))
        })
    }
    match e {
        Closure::Unit => Ans(Nop),
        Closure::Int(i) => Ans(Set(i as i32)),
        Closure::Float(d) => {
            /* TODO optimisation: erasure of duplicate constants */
            let l = id::L(id_gen.gen_id("l"));
            data.push((l.clone(), d));
            let x = id_gen.gen_id("l");
            Let(x.clone(), Type::Int, SetL(l),
                Box::new(Ans(LdDF(x, IdOrImm::C(0), 1))))
        },
        Closure::Neg(x) => Ans(Neg(x)),
        Closure::IntBin(op, x, y) => Ans(IntOp(op, x, IdOrImm::V(y))),
        Closure::FNeg(x) => Ans(FNegD(x)),
        Closure::FloatBin(op, x, y) => Ans(FloatOp(op, x, y)),
        Closure::IfComp(op, x, y, e1, e2) => {
            match env.get(&x) {
                Some(&Type::Bool) | Some(&Type::Int) => {
                    let nop = match op {
                        syntax::CompBin::Eq => CompBin::Eq,
                        syntax::CompBin::LE => CompBin::LE,
                    };
                    Ans(IfComp(nop, x, IdOrImm::V(y), invoke!(e1), invoke!(e2)))
                },
                Some(&Type::Float) => {
                    let nop = match op {
                        syntax::CompBin::Eq => FCompBin::Eq,
                        syntax::CompBin::LE => FCompBin::LE,
                    };
                    Ans(IfFComp(nop, x, y, invoke!(e1), invoke!(e2)))
                },
                _ => unreachable!(),
            }
        },
        Closure::Let((x, t1), e1, e2) => {
            let e1p = invoke!(e1);
            let mut cp_env = env.clone();
            cp_env.insert(x.clone(), t1.clone());
            let e2p = g(data, &cp_env, *e2, id_gen);
            concat(*e1p, x, t1, e2p)
        },
        Closure::Var(x) => {
            match env.get(&x) {
                Some(&Type::Unit) => Ans(Nop),
                Some(&Type::Float) => Ans(FMovD(x)),
                None => unreachable!(),
                _ => Ans(Mov(x)),
            }
        },
        /*
        TODO remaining (7):
    MakeCls(String, Type, Cls, Box<Closure>),
    AppCls(String, Box<[String]>),
    AppDir(id::L, Box<[String]>),
    Tuple(Box<[String]>),
    LetTuple(Box<[(String, Type)]>, String, Box<Closure>),
    Get(String, String),
    Put(String, String, String),
         */
        Closure::ExtArray(id::L(x)) =>
            Ans(SetL(id::L(format!("min_caml_{}", x)))),
        _ => panic!(),
    }
}

fn h(data: &mut Vec<(id::L, OrderedFloat<f64>)>, e: closure::Fundef,
     id_gen: &mut IdGen) -> Fundef {
    panic!();
}

pub fn f(closure::Prog(fundefs, e): closure::Prog, id_gen: &mut IdGen) -> Prog {
    let mut data = vec![];
    let fundefs = fundefs.into_vec()
        .into_iter().map(|u| h(&mut data, u, id_gen))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let e = g(&mut data, &HashMap::new(), e, id_gen);
    let data = data.into_iter()
        .map(|(l, f)| (l, f.into()))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    Prog(data, fundefs, e)
}
