use std::collections::HashMap;
use syntax::Type;
use id;
use id::IdGen;
use closure;
use closure::Closure;
use x86::asm::{Asm, Exp, IdOrImm, Fundef, Prog};
use ordered_float::OrderedFloat;

fn g(data: &mut Vec<(id::L, OrderedFloat<f64>)>, env: &HashMap<String, Type>,
     e: Closure, id_gen: &mut IdGen) -> Asm {
    use self::Asm::*;
    use self::Exp::*;
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
        /*
        TODO remaining (11):
    IfComp(CompBin, String, String, Box<Closure>, Box<Closure>),
    Let((String, Type), Box<Closure>, Box<Closure>),
    Var(String),
    MakeCls(String, Type, Cls, Box<Closure>),
    AppCls(String, Box<[String]>),
    AppDir(id::L, Box<[String]>),
    Tuple(Box<[String]>),
    LetTuple(Box<[(String, Type)]>, String, Box<Closure>),
    Get(String, String),
    Put(String, String, String),
    ExtArray(id::L),
         */
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
