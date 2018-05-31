use std::collections::HashMap;
use syntax;
use syntax::Type;
use id;
use id::IdGen;
use closure;
use closure::Closure;
use x86::asm;
use x86::asm::{Asm, CompBin, Exp, FCompBin, IdOrImm, Fundef, Prog};
use syntax::IntBin;
use ordered_float::OrderedFloat;

/*
 * S: State
 */
fn classify<S, T, F1: Fn(&mut S, T, String) -> T, F2: Fn(&mut S, T, String, Type) -> T>(state: &mut S, xts: Vec<(String, Type)>, init: T, addf: F1, addi: F2) -> T {
    let mut acc = init;
    for (x, t) in xts {
        acc = match t {
            Type::Unit => acc,
            Type::Float => addf(state, acc, x.clone()),
            _ => addi(state, acc, x.clone(), t),
        };
    }
    acc
}

fn separate(xts: &[(String, Type)]) -> (Box<[String]>, Box<[String]>) {
    let (intargs, floatargs)
        = classify(&mut (), xts.to_vec(), (Vec::new(), Vec::new()),
                   |_, (intargs, mut floatargs), x| { floatargs.push(x); (intargs, floatargs) },
                   |_, (mut intargs, floatargs), x, _| { intargs.push(x); (intargs, floatargs) });
    (intargs.into_boxed_slice(), floatargs.into_boxed_slice())
 }

fn expand<T, F1: Fn(&mut IdGen, String, i32, T) -> T, F2: Fn(&mut IdGen, String, Type, i32, T) -> T>(id_gen: &mut IdGen, xts: &[(String, Type)], init: (i32, T), addf: F1, addi: F2) -> (i32, T) {
    classify(id_gen, xts.to_vec(), init,
             |id_gen, (offset, acc), x| (offset + 8, addf(id_gen, x, offset, acc)),
             |id_gen, (offset, acc), x, t| (offset + 4, addi(id_gen, x, t, offset, acc)))
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
            asm::concat(*e1p, x, t1, e2p)
        },
        Closure::Var(x) => {
            match env.get(&x) {
                Some(&Type::Unit) => Ans(Nop),
                Some(&Type::Float) => Ans(FMovD(x)),
                None => unreachable!(),
                _ => Ans(Mov(x)),
            }
        },
        Closure::MakeCls(x, t, closure::Cls { entry: l, actual_fv: ys }, e2) => {
            use x86::asm::REG_HP;
            let mut copied_env = env.clone();
            copied_env.insert(x.clone(), t.clone());
            let e2p = g(data, &copied_env, *e2, id_gen);
            let ys = ys.into_vec().into_iter().map(|y| (y.clone(), env.get(&y).unwrap().clone())).collect::<Vec<_>>();
            let (offset, store_fv) = expand(id_gen, &ys,
                                            (4, e2p),
                                            |id_gen, y, offset, store_fv| asm::seq(id_gen, Exp::StDF(y, x.clone(), IdOrImm::C(offset), 1), store_fv),
                                            |id_gen, y, _, offset, store_fv| asm::seq(id_gen, Exp::St(y, x.clone(), IdOrImm::C(offset), 1), store_fv));
            let z = id_gen.gen_id("l");
            Asm::Let(x.clone(), t, Exp::Mov(REG_HP.to_string()),
                Box::new(Asm::Let(REG_HP.to_string(), Type::Int, Exp::IntOp(IntBin::Add, REG_HP.to_string(), IdOrImm::C(asm::align(offset))),
                             Box::new(Let(z.clone(), Type::Int, Exp::SetL(l),
                                          Box::new(asm::seq(id_gen, Exp::St(z, x.clone(), IdOrImm::C(0), 1), store_fv)))))))
        },
        Closure::AppCls(x, ys) => {
            let (intargs, floatargs) = separate(
                &ys.into_vec().into_iter()
                    .map(|y| {
                        let ty = env.get(&y).unwrap().clone(); (y, ty)
                    }).collect::<Vec<_>>());
            Asm::Ans(Exp::CallCls(x, intargs, floatargs))
        },
        Closure::AppDir(id::L(x), ys) => {
            let (intargs, floatargs) = separate(
                &ys.into_vec().into_iter()
                    .map(|y| {
                        let ty = env.get(&y).unwrap().clone(); (y, ty)
                    }).collect::<Vec<_>>());
            Asm::Ans(Exp::CallDir(id::L(x), intargs, floatargs))
        },
        /*
        TODO remaining (5):
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
    let inner_e = g(data, &inner_env, *e, id_gen);
    let (_offset, load) = expand(id_gen, &zts, (4, inner_e),
                                |_, z, offset, load| ::x86::asm::fletd(z, Exp::LdDF(x.clone(), IdOrImm::C(offset), 1), load),
                                |_, z, t, offset, load| Asm::Let(z, t, Exp::Ld(x.clone(), IdOrImm::C(offset), 1), Box::new(load)));
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
