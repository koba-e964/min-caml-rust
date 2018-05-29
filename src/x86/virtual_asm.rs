use std::collections::HashMap;
use syntax;
use syntax::Type;
use id;
use id::IdGen;
use closure;
use closure::Closure;
use x86::asm::{Asm, CompBin, Exp, FCompBin, IdOrImm, Fundef, Prog, concat};
use ordered_float::OrderedFloat;

fn classify<T, F1: Fn(T, String) -> T, F2: Fn(T, String, Type) -> T>(xts: Vec<(String, Type)>, init: T, addf: F1, addi: F2) -> T {
    let mut acc = init;
    for (x, t) in xts {
        acc = match t {
            Type::Unit => acc,
            Type::Float => addf(acc, x.clone()),
            _ => addi(acc, x.clone(), t),
        };
    }
    acc
}

fn separate(xts: &[(String, Type)]) -> (Box<[String]>, Box<[String]>) {
    let (intargs, floatargs)
        = classify(xts.to_vec(), (Vec::new(), Vec::new()),
                   |(intargs, mut floatargs), x| { floatargs.push(x); (intargs, floatargs) },
                   |(mut intargs, floatargs), x, _| { intargs.push(x); (intargs, floatargs) });
    (intargs.into_boxed_slice(), floatargs.into_boxed_slice())
}

fn expand<T, F1: Fn(String, i32, T) -> T, F2: Fn(String, Type, i32, T) -> T>(xts: &[(String, Type)], init: (i32, T), addf: F1, addi: F2) -> (i32, T) {
    classify(xts.to_vec(), init,
             |(offset, acc), x| (offset + 8, addf(x, offset, acc)),
             |(offset, acc), x, t| (offset + 4, addi(x, t, offset, acc)))
}

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
        TODO remaining (6):
    MakeCls(String, Type, Cls, Box<Closure>),
        AppCls(String, Box<[String]>),
         */
        Closure::AppDir(id::L(x), ys) => {
            let (intargs, floatargs) = separate(&ys.into_vec().into_iter().map(|y| { let ty = env.get(&y).unwrap().clone(); (y, ty) }).collect::<Vec<_>>());
            Asm::Ans(Exp::CallDir(id::L(x), intargs, floatargs))
        },
        /*
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

fn h(data: &mut Vec<(id::L, OrderedFloat<f64>)>,
     closure::Fundef { name: (id::L(x), t), args: yts, formal_fv: zts, body: e }: closure::Fundef,
     id_gen: &mut IdGen) -> Fundef {
    let (intargs, floatargs) = separate(&yts);
    let mut inner_env = HashMap::new();
    for &(ref z, ref t) in zts.iter() {
        inner_env.insert(z.clone(), t.clone());
    }
    for &(ref y, ref t) in yts.iter() {
        inner_env.insert(y.clone(), t.clone());
    }
    inner_env.insert(x.clone(), t.clone());
    let (_offset, load) = expand(&zts, (4, g(data, &inner_env, *e, id_gen)),
                                |z, offset, load| ::x86::asm::fletd(z, Exp::LdDF(x.clone(), IdOrImm::C(offset), 1), load),
                                |z, t, offset, load| Asm::Let(z, t, Exp::Ld(x.clone(), IdOrImm::C(offset), 1), Box::new(load)));
    match t {
        Type::Fun(_, t2) => Fundef { name: id::L(x), args: intargs, fargs: floatargs, body: load, ret: *t2 },
        _ => unreachable!(),
    }
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
