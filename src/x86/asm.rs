use syntax::{IntBin, FloatBin, Type};
use id;

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
    FloatOp(FloatBin, String, String),
    LdDF(String, IdOrImm, i32),
    StDF(String, String, IdOrImm, i32),
    Comment(String),
    /* virtual instructions */
    IfComp(CompBin, String, IdOrImm, Box<Exp>, Box<Exp>),
    IfFComp(FCompBin, String, IdOrImm, Box<Exp>, Box<Exp>),
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

