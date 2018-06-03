use syntax::{IntBin, FloatBin, Type};
use id;
use std::collections::{HashSet};
use std::fmt;

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
    IfComp(CompBin, String, IdOrImm, Box<Asm>, Box<Asm>),
    IfFComp(FCompBin, String, String, Box<Asm>, Box<Asm>),
    /* Closure address, integer arguments float arguments */
    CallCls(String, Box<[String]>, Box<[String]>),
    CallDir(id::L, Box<[String]>, Box<[String]>),
    Save(String, String),
    Restore(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fundef {
    pub name: id::L,
    pub args: Box<[String]>,
    pub fargs: Box<[String]>,
    pub body: Asm,
    pub ret: Type,
}

#[derive(Debug, Clone)]
pub struct Prog(pub Box<[(id::L, f64)]>, pub Box<[Fundef]>, pub Asm);

// Display
impl Asm {
    fn fmt2(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        match self {
            &Asm::Ans(ref e) => write!(f, "ret {:?}", e),
            &Asm::Let(ref x, ref t, ref e1, ref e2) => {
                write!(f, "let {}: {} = {} in\n", x, t, e1)?;
                for _ in 0 .. level {
                    write!(f, " ")?;
                }
                write!(f, "{}", e2)
            },
        }
    }
}
impl Exp {
    fn fmt2(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        // TODO
        write!(f, "{:?}", self)
    }
}
impl fmt::Display for Asm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt2(f, 0)
    }
}
impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt2(f, 0)
    }
}
impl fmt::Display for Fundef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let &Fundef { name: id::L(ref name), ref args, ref fargs, ref body, ref ret }
        = self;
        write!(f, "asm-define {}", name)?;
        for y in args.iter() {
            write!(f, " ({}: intptr)", y)?;
        }
        for y in fargs.iter() {
            write!(f, " ({}: float)", y)?;
        }
        write!(f, ":{} {{\n  ", ret)?;
        body.fmt2(f, 2)?;
        write!(f, "\n}}")
    }
}
impl fmt::Display for Prog {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let &Prog(ref data, ref fundefs, ref e) = self;
        for &(id::L(ref name), ref value) in data.iter() {
            write!(f, "{} => {}\n", name, value)?;
        }
        for fundef in fundefs.iter() {
            write!(f, "{}\n", fundef)?;
        }
        write!(f, "{}", e)
    }
}

pub fn fletd(x: String, e1: Exp, e2: Asm) -> Asm {
    Asm::Let(x, Type::Float, e1, Box::new(e2))
}

pub fn seq(id_gen: &mut id::IdGen, e1: Exp, e2: Asm) -> Asm {
    let id = id_gen.gen_tmp(&Type::Unit);
    Asm::Let(id, Type::Unit, e1, Box::new(e2))
}

const REGS: [&str; 6] = ["%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi"];

pub fn fregs() -> Vec<String> {
    let mut res = vec!["".to_string(); 8];
    for i in 0 .. 8 {
        res[i] = format!("%xmm{}", i);
    }
    res
}

/// closure address (caml2html: sparcasm_regcl)
pub fn reg_cl() -> &'static str {
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
            let s1 = fv(e1);
            let s2 = fv(e2);
            &(&h | &s1) | &(&s2 | &fv_id_or_imm(yp))
        },
        Exp::IfFComp(_, ref x, ref y, ref e1, ref e2) => {
            let h = build_set!(x, y);
            let s1 = fv(e1);
            let s2 = fv(e2);
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

pub fn align(i: i32) -> i32 {
    if i % 8 == 0 {
        i
    } else {
        i + 4
    }
}
