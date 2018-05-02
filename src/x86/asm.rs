use syntax::{IntBin, FloatBin, Type};
use id;
use std::collections::{HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IdOrImm {
    V(String),
    C(i32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Asm {
    Ans(Exp),
    Let(String, Type, Exp, Box<Asm>),
}

// We have GE because x86 instructions are not symmetric
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompBin {
    Eq,
    LE,
    GE,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FCompBin {
    Eq,
    LE,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp {
    Nop,
    Set(i32),
    SetL(id::L),
    Mov(String),
    Neg(String),
    IntOp(IntBin, String, IdOrImm),
    Ld(String, IdOrImm, i32),
    St(String, String, IdOrImm, i32),
    FMovD(String),
    FNegD(String),
    FloatOp(FloatBin, String, String),
    LdDF(String, IdOrImm, i32),
    StDF(String, String, IdOrImm, i32),
    Comment(String),
    /* virtual instructions */
    IfComp(CompBin, String, IdOrImm, Box<Exp>, Box<Exp>),
    IfFComp(FCompBin, String, String, Box<Exp>, Box<Exp>),
    /* Closure address, integer arguments float arguments */
    CallCls(String, Box<[String]>, Box<[String]>),
    CallDir(id::L, Box<[String]>, Box<[String]>),
    Save(String, String),
    Restore(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fundef {
    name: id::L,
    args: Box<[String]>,
    fargs: Box<[String]>,
    body: Asm,
    ret: Type,
}

pub struct Prog(Box<[(id::L, f64)]>, Box<[Fundef]>, Asm);

fn fletd(x: String, e1: Exp, e2: Asm) -> Asm {
    Asm::Let(x, Type::Float, e1, Box::new(e2))
}

fn seq(id_gen: &mut id::IdGen, e1: Exp, e2: Asm) -> Asm {
    let id = id_gen.gen_tmp(&Type::Unit);
    Asm::Let(id, Type::Unit, e1, Box::new(e2))
}

const REGS: [&str; 6] = ["%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi"];

fn fregs() -> Vec<String> {
    let mut res = vec!["".to_string(); 8];
    for i in 0 .. 8 {
        res[i] = format!("%xmm{}", i);
    }
    res
}

/// closure address (caml2html: sparcasm_regcl)
fn reg_cl() -> &'static str {
    REGS[REGS.len() - 1]
}

pub const REG_SP: &str = "%ebp";
pub const REG_HP: &str = "min_caml_hp";

pub fn is_reg(x: &str) -> bool {
    let c = x.chars().nth(0);
    c == Some('%') || x == REG_HP
}

/// free variables in the order of use (for spilling) (caml2html: sparcasm_fv)
fn fv_id_or_imm(x: &IdOrImm) -> HashSet<String> {
    match x {
        &IdOrImm::V(ref x) => build_set!(x),
        &IdOrImm::C(_) => build_set!(),
    }
}

/// ys: integral parameters, zs: float parameters. Taylor-made for the following fn fv_exp().
fn fv_parameters(ys: &[String], zs: &[String]) -> HashSet<String> {
    &ys.iter().cloned().collect::<HashSet<_>>() |
    &zs.iter().cloned().collect::<HashSet<_>>()
}

fn fv_exp(x: &Exp) -> HashSet<String> {
    macro_rules! invoke {
        ($e:expr) => (fv_exp($e));
    }
    match *x {
        Exp::Nop | Exp::Set(_) |
        Exp::SetL(_) | Exp::Comment(_) | Exp::Restore(_) => build_set!(),
        Exp::Mov(ref x) | Exp::Neg(ref x) |
        Exp::FMovD(ref x) | Exp::FNegD(ref x) | Exp::Save(ref x, _) =>
            build_set!(x),
        Exp::IntOp(_, ref x, ref yp) | Exp::Ld(ref x, ref yp, _) |
        Exp::LdDF(ref x, ref yp, _) => {
            let mut ret = fv_id_or_imm(yp);
            ret.insert(x.to_string());
            ret
        },
        Exp::St(ref x, ref y, ref zp, _) |
        Exp::StDF(ref x, ref y, ref zp, _) => {
            let mut ret = fv_id_or_imm(zp);
            ret.insert(x.to_string());
            ret.insert(y.to_string());
            ret
        },
        Exp::FloatOp(_, ref x, ref y) =>
            build_set!(x, y),
        Exp::IfComp(_, ref x, ref yp, ref e1, ref e2) => {
            let h = build_set!(x);
            let s1 = invoke!(e1);
            let s2 = invoke!(e2);
            &(&h | &s1) | &(&s2 | &fv_id_or_imm(yp))
        },
        Exp::IfFComp(_, ref x, ref y, ref e1, ref e2) => {
            let h = build_set!(x, y);
            let s1 = invoke!(e1);
            let s2 = invoke!(e2);
            &(&h | &s1) | &s2
        },
        Exp::CallCls(ref x, ref ys, ref zs) =>
            &build_set!(x) | &fv_parameters(ys, zs),
        Exp::CallDir(_, ref ys, ref zs) => fv_parameters(ys, zs),
    }
}

pub fn fv(e: &Asm) -> HashSet<String> {
    match *e {
        Asm::Ans(ref exp) => fv_exp(exp),
        Asm::Let(ref x, _, ref exp, ref e) =>
            &fv_exp(exp) | &(&fv(e) - &build_set!(x)),
    }
}

pub fn concat(e1: Asm, x: String, t: Type, e2: Asm) -> Asm {
    match e1 {
        Asm::Ans(exp) => Asm::Let(x, t, exp, Box::new(e2)),
        Asm::Let(y, yt, exp, e1p) =>
            Asm::Let(y, yt, exp, Box::new(concat(*e1p, x, t, e2))),
    }
}

fn align(i: i32) -> i32 {
    if i % 8 == 0 {
        i
    } else {
        i + 4
    }
}
