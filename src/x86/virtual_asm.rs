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
                Some(Type::Bool) | Some(Type::Int) => {
                    let nop = match op {
                        syntax::CompBin::Eq => CompBin::Eq,
                        syntax::CompBin::LE => CompBin::LE,
                    };
                    Ans(IfComp(nop, x, IdOrImm::V(y), invoke!(e1), invoke!(e2)))
                },
                Some(Type::Float) => {
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
                Some(Type::Unit) => Ans(Nop),
                Some(Type::Float) => Ans(FMovD(x)),
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
        Closure::Tuple(xs) => {
            use x86::asm::{REG_HP,align};
            let y = id_gen.gen_id("t");
            let xs_with_type = xs.iter().map(|x| { let tmp = env.get(x).unwrap(); (x.clone(), tmp.clone()) }).collect::<Vec<_>>();
            let (offset, store) = expand(id_gen, &xs_with_type,
                                         (0, Ans(Mov(y.clone()))),
                                         |id_gen, x, offset, store| asm::seq(id_gen, StDF(x, y.clone(), IdOrImm::C(offset), 1), store),
                                         |id_gen, x, _, offset, store| asm::seq(id_gen, St(x, y.clone(), IdOrImm::C(offset), 1), store));
            Let(y, Type::Tuple(xs.iter().map(|x| env.get(x).unwrap().clone()).collect::<Vec<_>>().into_boxed_slice()),
                Mov(REG_HP.to_string()),
                Box::new(Let(REG_HP.to_string(), Type::Int,
                             IntOp(IntBin::Add, REG_HP.to_string(), IdOrImm::C(align(offset))),
                             Box::new(store))))
        },
        Closure::LetTuple(xts, y, e2) => {
            use self::asm::fletd;
            let s = closure::fv(&e2);
            let mut copied_env = env.clone();
            for (x, t) in xts.iter() {
                copied_env.insert(x.clone(), t.clone());
            }
            let init = (0, g(data, &copied_env, *e2, id_gen));
            let (_, load) = expand(
                id_gen, &xts, init,
                /* [XX] a little ad hoc optimization */
                |_, x, offset, load| if !s.contains(&x) { load } else {
                    fletd(x, LdDF(y.clone(), IdOrImm::C(offset), 1), load) },
                /* [XX] a little ad hoc optimization */
                |_, x, t, offset, load| if !s.contains(&x) { load } else {
                    Let(x, t, Ld(y.clone(), IdOrImm::C(offset), 1), Box::new(load)) });
            load
        },
        Closure::Get(x, y) =>
            match env.get(&x) {
                Some(Type::Array(ty)) => match ty as &Type {
                    Type::Unit => Asm::Ans(Exp::Nop),
                    Type::Float =>
                        Asm::Ans(Exp::LdDF(x, IdOrImm::V(y), 8)),
                    _ =>
                        Asm::Ans(Exp::Ld(x, IdOrImm::V(y), 4)),
                },
                _ => panic!("invalid type for an array (Get)"),
            }
        Closure::Put(x, y, z) =>
            match env.get(&x) {
                Some(Type::Array(ty)) => match ty as &Type {
                    Type::Unit => Asm::Ans(Exp::Nop),
                    Type::Float =>
                        Asm::Ans(Exp::StDF(z, x, IdOrImm::V(y), 8)),
                    _ =>
                        Asm::Ans(Exp::St(z, x, IdOrImm::V(y), 4)),
                },
                _ => panic!("invalid type for an array (Put)"),
            }
        Closure::ExtArray(id::L(x)) =>
            Ans(SetL(id::L(format!("min_caml_{}", x)))),
    }
}

fn h(data: &mut Vec<(id::L, OrderedFloat<f64>)>,
     closure::Fundef { name: (id::L(x), t), args: yts, formal_fv: zts, body: e }: closure::Fundef,
     id_gen: &mut IdGen) -> Fundef {
    let (intargs, floatargs) = separate(&yts);
    let mut inner_env = HashMap::new();
    for (z, t) in zts.iter() {
        inner_env.insert(z.clone(), t.clone());
    }
    for (y, t) in yts.iter() {
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

#[cfg(test)]
mod tests {
    use x86::virtual_asm::*;
    use closure::*;
    #[test]
    fn test_comparison() {
        use std::collections::HashMap;

        let mut data = Vec::new();
        let mut env = HashMap::new();
        let x = || "x".to_string();
        let y = || "y".to_string();
        env.insert(x(), Type::Int);
        env.insert(y(), Type::Int);
        let e = Closure::IfComp(syntax::CompBin::LE, x(), y(),
                                Box::new(Closure::Int(3)),
                                Box::new(Closure::Int(7)));
        let mut id_gen = IdGen::new();
        let int_wrapper = |value| Box::new(Asm::Ans(Exp::Set(value)));
        let expected = Asm::Ans(
            Exp::IfComp(CompBin::LE, x(), IdOrImm::V(y()),
                        int_wrapper(3), int_wrapper(7)));
        assert_eq!(expected, g(&mut data, &env, e, &mut id_gen));
    }
    #[test]
    fn test_make_closure() {
        use std::collections::HashMap;

        let mut data = Vec::new();
        let mut env = HashMap::new();
        let x = || "x".to_string();
        let y = || "y".to_string();
        let func = || "f".to_string();
        let func_type = Type::Fun(Box::new([Type::Int]), Box::new(Type::Int));
        env.insert(x(), Type::Int);
        env.insert(y(), Type::Int);
        let e = Closure::MakeCls(func(),
                                 func_type.clone(),
                                 Cls { entry: id::L(func()),
                                       actual_fv: Box::new([x(), y()]),
                                 },
                                 Box::new(Closure::Unit));
        let mut id_gen = IdGen::new();
        let expected;
        {
            use syntax::IntBin;

            let e5 = Asm::Ans(Exp::Nop);
            // Note that g() may change its behavior. Temporary variables' names are therefore random.
            let e4 = Asm::Let("u0".to_string(), Type::Unit, Exp::St(x(), func(), IdOrImm::C(4), 1), Box::new(e5));
            let e3 = Asm::Let("u1".to_string(), Type::Unit, Exp::St(y(), func(), IdOrImm::C(8), 1), Box::new(e4));
            let e2 = Asm::Let("u3".to_string(), Type::Unit, Exp::St("l2".to_string(), func(), IdOrImm::C(0), 1), Box::new(e3));
            let e1 = Asm::Let("l2".to_string(), Type::Int, Exp::SetL(id::L("f".to_string())), Box::new(e2));
            let e0 = Asm::Let("min_caml_hp".to_string(), Type::Int, Exp::IntOp(IntBin::Add, "min_caml_hp".to_string(), IdOrImm::C(16)), Box::new(e1));
            expected = Asm::Let(func(), func_type, Exp::Mov("min_caml_hp".to_string()), Box::new(e0));
        }
        assert_eq!(expected, g(&mut data, &env, e, &mut id_gen));
    }
}
