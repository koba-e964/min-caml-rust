use crate::syntax::IntBin;
use crate::x86::asm;
use crate::x86::asm::{Asm, CompBin, Exp, Fundef, IdOrImm, Prog};
use std::collections::HashMap;

// Name -> Const
type ConstEnv = HashMap<String, i32>;

// Variables are alpha-converted, so env can be reused.
fn g(env: &mut ConstEnv, e: Asm) -> Asm {
    match e {
        Asm::Ans(exp) => Asm::Ans(g_p(env, exp)),
        Asm::Let(x, t, Exp::Set(i), e) => {
            env.insert(x.clone(), i);
            let e = g(env, *e);
            if asm::fv(&e).contains(&x) {
                Asm::Let(x, t, Exp::Set(i), Box::new(e))
            } else {
                e
            }
        }
        Asm::Let(x, t, exp, e) => Asm::Let(x, t, g_p(env, exp), Box::new(g(env, *e))),
    }
}

fn g_p(env: &mut ConstEnv, e: Exp) -> Exp {
    macro_rules! invoke {
        ($e: expr) => {
            Box::new(g(env, *$e))
        };
    }
    match e {
        Exp::Mov(ref x) if env.contains_key(x) => Exp::Set(env[x]),
        Exp::IntOp(op, ref x, IdOrImm::V(ref y)) if env.contains_key(y) => {
            Exp::IntOp(op, x.clone(), IdOrImm::C(env[y]))
        }
        // x + y = y + x
        Exp::IntOp(IntBin::Add, ref x, IdOrImm::V(ref y)) if env.contains_key(x) => {
            Exp::IntOp(IntBin::Add, y.clone(), IdOrImm::C(env[x]))
        }
        Exp::Ld(ref x, IdOrImm::V(ref y), imm) if env.contains_key(y) => {
            Exp::Ld(x.clone(), IdOrImm::C(env[y]), imm)
        }
        Exp::St(ref x, ref y, IdOrImm::V(ref z), imm) if env.contains_key(z) => {
            Exp::St(x.clone(), y.clone(), IdOrImm::C(env[z]), imm)
        }
        Exp::LdDF(ref x, IdOrImm::V(ref y), i) if env.contains_key(y) => {
            Exp::LdDF(x.clone(), IdOrImm::C(env[y]), i)
        }
        Exp::StDF(ref x, ref y, IdOrImm::V(ref z), i) if env.contains_key(z) => {
            Exp::StDF(x.clone(), y.clone(), IdOrImm::C(env[z]), i)
        }
        Exp::IfComp(op, x, y_p, e1, e2) => {
            if let IdOrImm::V(ref y) = y_p {
                if let Some(&env_y) = env.get(y) {
                    return Exp::IfComp(op, x, IdOrImm::C(env_y), invoke!(e1), invoke!(e2));
                }
                if let Some(&env_x) = env.get(&x) {
                    let opposite = match op {
                        CompBin::Eq => CompBin::Eq,
                        CompBin::LE => CompBin::GE,
                        CompBin::GE => CompBin::LE,
                    };
                    return Exp::IfComp(
                        opposite,
                        y.clone(),
                        IdOrImm::C(env_x),
                        invoke!(e1),
                        invoke!(e2),
                    );
                }
            }
            Exp::IfComp(op, x, y_p, invoke!(e1), invoke!(e2))
        }
        Exp::IfFComp(op, x, y_p, e1, e2) => Exp::IfFComp(op, x, y_p, invoke!(e1), invoke!(e2)),
        _ => e,
    }
}

fn h(mut fundef: Fundef) -> Fundef {
    let mut env = HashMap::new();
    fundef.body = g(&mut env, fundef.body);
    eprintln!("h_env = {:?}", env);
    fundef
}

pub fn f(Prog(data, fundefs, e): Prog) -> Prog {
    Prog(
        data,
        fundefs
            .into_vec()
            .into_iter()
            .map(h)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        {
            let mut env = HashMap::new();
            let ans = g(&mut env, e);
            eprintln!("env = {:?}", env);
            ans
        },
    )
}

#[cfg(test)]
mod tests {
    use crate::syntax::{IntBin, Type};
    use crate::x86::asm::{Asm, CompBin, Exp, FCompBin, IdOrImm, Prog};
    use crate::x86::simm;
    // Checks if g(a) == b. Since g is not public, a and b are wrapped in Prog.
    fn assert_g_works(a: Asm, b: Asm) {
        let e1 = Prog(Box::new([]), Box::new([]), a);
        let e2 = Prog(Box::new([]), Box::new([]), b);
        assert_eq!(simm::f(e1), e2);
    }
    #[test]
    fn imm_in_mov_is_inlined() {
        let x = || "x".to_string();
        let e1 = Asm::Let(
            x(),
            Type::Int,
            Exp::Set(5),
            Box::new(Asm::Ans(Exp::Mov(x()))),
        );
        let e2 = Asm::Ans(Exp::Set(5));
        assert_g_works(e1, e2);
    }
    #[test]
    fn imm_in_add_is_inlined() {
        let x = || "x".to_string();
        let y = || "y".to_string();
        let e1 = Asm::Ans(Exp::IntOp(IntBin::Add, x(), IdOrImm::V(y())));
        let e1 = Asm::Let(y(), Type::Int, Exp::Set(5), Box::new(e1));
        let e2 = Asm::Ans(Exp::IntOp(IntBin::Add, x(), IdOrImm::C(5)));
        assert_g_works(e1, e2);
    }
    #[test]
    fn imm_in_add_first_is_inlined() {
        let x = || "x".to_string();
        let y = || "y".to_string();
        let e1 = Asm::Ans(Exp::IntOp(IntBin::Add, x(), IdOrImm::V(y())));
        let e1 = Asm::Let(x(), Type::Int, Exp::Set(7), Box::new(e1));
        let e2 = Asm::Ans(Exp::IntOp(IntBin::Add, y(), IdOrImm::C(7)));
        assert_g_works(e1, e2);
    }
    #[test]
    fn imm_in_comp_is_inlined() {
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = Asm::Ans(Exp::Mov("z".to_string()));
        let w = Asm::Ans(Exp::Mov("w".to_string()));
        let e1 = Asm::Ans(Exp::IfComp(
            CompBin::Eq,
            x(),
            IdOrImm::V(y()),
            Box::new(z.clone()),
            Box::new(w.clone()),
        ));
        let e1 = Asm::Let(y(), Type::Int, Exp::Set(5), Box::new(e1));
        let e2 = Asm::Ans(Exp::IfComp(
            CompBin::Eq,
            x(),
            IdOrImm::C(5),
            Box::new(z),
            Box::new(w),
        ));
        assert_g_works(e1, e2);
    }
    #[test]
    fn imm_in_comp_first_is_inlined() {
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = Asm::Ans(Exp::Mov("z".to_string()));
        let w = Asm::Ans(Exp::Mov("w".to_string()));
        let e1 = Asm::Ans(Exp::IfComp(
            CompBin::GE,
            x(),
            IdOrImm::V(y()),
            Box::new(z.clone()),
            Box::new(w.clone()),
        ));
        let e1 = Asm::Let(x(), Type::Int, Exp::Set(7), Box::new(e1));
        let e2 = Asm::Ans(Exp::IfComp(
            CompBin::LE,
            y(),
            IdOrImm::C(7),
            Box::new(z),
            Box::new(w),
        ));
        assert_g_works(e1, e2);
    }
    #[test]
    fn imm_in_then_clause_of_comp_is_inlined() {
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = || "z".to_string();
        let w = Asm::Ans(Exp::Mov("w".to_string()));
        let e1 = Asm::Let(
            x(),
            Type::Int,
            Exp::Set(5),
            Box::new(Asm::Ans(Exp::Mov(x()))),
        );
        let e1 = Asm::Ans(Exp::IfComp(
            CompBin::Eq,
            y(),
            IdOrImm::V(z()),
            Box::new(e1),
            Box::new(w.clone()),
        ));
        let e2 = Asm::Ans(Exp::Set(5));
        let e2 = Asm::Ans(Exp::IfComp(
            CompBin::Eq,
            y(),
            IdOrImm::V(z()),
            Box::new(e2),
            Box::new(w.clone()),
        ));
        assert_g_works(e1, e2);
    }
    #[test]
    fn imm_in_then_clause_of_fcomp_is_inlined() {
        let x = || "x".to_string();
        let y = || "y".to_string();
        let z = || "z".to_string();
        let w = Asm::Ans(Exp::Mov("w".to_string()));
        let e1 = Asm::Let(
            x(),
            Type::Int,
            Exp::Set(5),
            Box::new(Asm::Ans(Exp::Mov(x()))),
        );
        let e1 = Asm::Ans(Exp::IfFComp(
            FCompBin::Eq,
            y(),
            z(),
            Box::new(e1),
            Box::new(w.clone()),
        ));
        let e2 = Asm::Ans(Exp::Set(5));
        let e2 = Asm::Ans(Exp::IfFComp(
            FCompBin::Eq,
            y(),
            z(),
            Box::new(e2),
            Box::new(w.clone()),
        ));
        assert_g_works(e1, e2);
    }
}
