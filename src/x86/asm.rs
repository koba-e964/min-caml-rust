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
    // Ld(x, y, disp) -> dword ptr [x + y * disp]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Prog(pub Box<[(id::L, f64)]>, pub Box<[Fundef]>, pub Asm);

// Display
impl Asm {
    fn fmt2(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        match self {
            Asm::Ans(e) => {
                write!(f, "ret ")?;
                e.fmt2(f, level)
            },
            Asm::Let(x, t, e1, e2) => {
                write!(f, "let {}: {} = ", x, t)?;
                e1.fmt2(f, level)?;
                writeln!(f, " in")?;
                for _ in 0 .. level {
                    write!(f, " ")?;
                }
                e2.fmt2(f, level)
            },
        }
    }
}
impl Exp {
    fn fmt2(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        match self {
            Exp::Nop => write!(f, "nop"),
            Exp::Set(i) => write!(f, "set {}", i),
            Exp::SetL(id::L(l)) => write!(f, "setl {}", l),
            Exp::Mov(x) => write!(f, "set {}", x),
            Exp::Neg(x) => write!(f, "neg {}", x),
            Exp::IntOp(op, x, y) => {
                let op_string = match op {
                    IntBin::Add => "add",
                    IntBin::Sub => "sub",
                };
                write!(f, "{} {} {}", op_string, x, y)
            },
            Exp::Ld(x, y, offset) => write!(f, "ld {} {} {}", x, y, offset),
            Exp::St(z, x, y, offset) =>
                write!(f, "st {} {} {} {}", z, x, y, offset),
            Exp::FMovD(x) => write!(f, "fmov {}", x),
            Exp::FNegD(x) => write!(f, "fneg {}", x),
            Exp::FloatOp(op, x, y) => {
                let op_string = match op {
                    FloatBin::FAdd => "fadd",
                    FloatBin::FSub => "fsub",
                    FloatBin::FMul => "fmul",
                    FloatBin::FDiv => "fdiv",
                };
                write!(f, "{} {} {}", op_string, x, y)
            },
            Exp::LdDF(x, y, offset) => write!(f, "lddf {} {} {}", x, y, offset),
            Exp::StDF(z, x, y, offset) =>
                write!(f, "stdf {} {} {} {}", z, x, y, offset),
            Exp::Comment(comment) => write!(f, ";; {}", comment),
            Exp::IfComp(op, x, y, e1, e2) => {
                let op_string = match op {
                    CompBin::Eq => "eq",
                    CompBin::LE => "le",
                    CompBin::GE => "ge",
                };
                writeln!(f, "if {} {} {} then", op_string, x, y)?;
                for _ in 0 .. level + 2 {
                    write!(f, " ")?;
                }
                e1.fmt2(f, level + 2)?;
                writeln!(f)?;
                for _ in 0 .. level {
                    write!(f, " ")?;
                }
                writeln!(f, "else")?;
                for _ in 0 .. level + 2 {
                    write!(f, " ")?;
                }
                e2.fmt2(f, level + 2)
            },
            Exp::IfFComp(op, x, y, e1, e2) => {
                let op_string = match op {
                    FCompBin::Eq => "feq",
                    FCompBin::LE => "fle",
                };
                writeln!(f, "if {} {} {} then", op_string, x, y)?;
                for _ in 0 .. level + 2 {
                    write!(f, " ")?;
                }
                e1.fmt2(f, level + 2)?;
                writeln!(f)?;
                for _ in 0 .. level {
                    write!(f, " ")?;
                }
                writeln!(f, "else")?;
                for _ in 0 .. level + 2 {
                    write!(f, " ")?;
                }
                e2.fmt2(f, level + 2)
            },
            Exp::CallCls(name, ys, zs) => {
                write!(f, "[{}]", name)?;
                for v in ys.iter() {
                    write!(f, " {}", v)?;
                }
                write!(f, ",")?;
                for v in zs.iter() {
                    write!(f, " {}", v)?;
                }
                Ok(())
            },
            Exp::CallDir(id::L(name), ys, zs) => {
                write!(f, "{}", name)?;
                for v in ys.iter() {
                    write!(f, " {}", v)?;
                }
                for v in zs.iter() {
                    write!(f, " fl:{}", v)?;
                }
                Ok(())
            },
            Exp::Save(varname, regname) =>
                write!(f, "SAVE {} {}", varname, regname),
            Exp::Restore(x) => write!(f, "restore {}", x),
        }
    }
}
impl fmt::Display for IdOrImm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IdOrImm::V(x) => write!(f, "{}", x),
            IdOrImm::C(imm) => write!(f, "{}", imm),
        }
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
        let Fundef { name: id::L(name), args, fargs, body, ret }
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
        let Prog(data, fundefs, e) = self;
        for (id::L(name), value) in data.iter() {
            writeln!(f, "{} => {}", name, value)?;
        }
        for fundef in fundefs.iter() {
            writeln!(f, "{}", fundef)?;
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
    for (i, item) in res.iter_mut().enumerate() {
        *item = format!("%xmm{}", i);
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
        IdOrImm::V(x) => build_set!(x),
        IdOrImm::C(_) => build_set!(),
    }
}

/// ys: integral parameters, zs: float parameters. Taylor-made for the following fn fv_exp().
fn fv_parameters(ys: &[String], zs: &[String]) -> HashSet<String> {
    &ys.iter().cloned().collect::<HashSet<_>>() |
    &zs.iter().cloned().collect::<HashSet<_>>()
}

fn fv_exp(x: &Exp) -> HashSet<String> {
    match x {
        Exp::Nop | Exp::Set(_) |
        Exp::SetL(_) | Exp::Comment(_) | Exp::Restore(_) => build_set!(),
        Exp::Mov(x) | Exp::Neg(x) |
        Exp::FMovD(x) | Exp::FNegD(x) | Exp::Save(x, _) =>
            build_set!(x),
        Exp::IntOp(_, x, yp) | Exp::Ld(x, yp, _) |
        Exp::LdDF(x, yp, _) => {
            let mut ret = fv_id_or_imm(yp);
            ret.insert(x.to_string());
            ret
        },
        Exp::St(x, y, zp, _) |
        Exp::StDF(x, y, zp, _) => {
            let mut ret = fv_id_or_imm(zp);
            ret.insert(x.to_string());
            ret.insert(y.to_string());
            ret
        },
        Exp::FloatOp(_, x, y) =>
            build_set!(x, y),
        Exp::IfComp(_, x, yp, e1, e2) => {
            let h = build_set!(x);
            let s1 = fv(e1);
            let s2 = fv(e2);
            &(&h | &s1) | &(&s2 | &fv_id_or_imm(yp))
        },
        Exp::IfFComp(_, x, y, e1, e2) => {
            let h = build_set!(x, y);
            let s1 = fv(e1);
            let s2 = fv(e2);
            &(&h | &s1) | &s2
        },
        Exp::CallCls(x, ys, zs) =>
            &build_set!(x) | &fv_parameters(ys, zs),
        Exp::CallDir(_, ys, zs) => fv_parameters(ys, zs),
    }
}

pub fn fv(e: &Asm) -> HashSet<String> {
    match e {
        Asm::Ans(exp) => fv_exp(exp),
        Asm::Let(x, _, exp, e) =>
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
