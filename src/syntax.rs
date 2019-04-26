use ordered_float::OrderedFloat;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
// syntax.ml
pub enum Syntax {
    Unit,
    Bool(bool),
    Int(i64),
    Float(OrderedFloat<f64>),
    Not(Box<Syntax>),
    Neg(Box<Syntax>),
    IntBin(IntBin, Box<Syntax>, Box<Syntax>),
    FNeg(Box<Syntax>),
    FloatBin(FloatBin, Box<Syntax>, Box<Syntax>),
    CompBin(CompBin, Box<Syntax>, Box<Syntax>),
    If(Box<Syntax>, Box<Syntax>, Box<Syntax>),
    Let((String, Type), Box<Syntax>, Box<Syntax>),
    Var(String),
    LetRec(Fundef, Box<Syntax>),
    App(Box<Syntax>, Box<[Syntax]>),
    Tuple(Box<[Syntax]>),
    LetTuple(Box<[(String, Type)]>, Box<Syntax>, Box<Syntax>),
    Array(Box<Syntax>, Box<Syntax>),
    Get(Box<Syntax>, Box<Syntax>),
    Put(Box<Syntax>, Box<Syntax>, Box<Syntax>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fundef {
    pub name: (String, Type),
    pub args: Box<[(String, Type)]>,
    pub body: Box<Syntax>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntBin {
    Add,
    Sub,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatBin {
    FAdd,
    FSub,
    FMul,
    FDiv,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompBin {
    Eq,
    LE,
}

// type.ml
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Box<[Type]>, Box<Type>),
    Tuple(Box<[Type]>),
    Array(Box<Type>),
    Var(usize),
}

impl Type {
    fn fmt_sub(&self, f: &mut fmt::Formatter, parens: bool) -> fmt::Result {
        use self::Type::*;
        match self {
            Unit => write!(f, "()"),
            Bool => write!(f, "bool"),
            Int => write!(f, "int"),
            Float => write!(f, "float"),
            Fun(argty, retty) => {
                write!(f, "{{")?;
                for i in 0..argty.len() {
                    write!(f, "{}", argty[i])?;
                    if i < argty.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")?;
                write!(f, " -> {}", retty)
            }
            Tuple(xs) => {
                if parens {
                    write!(f, "(")?;
                }
                for i in 0..xs.len() {
                    xs[i].fmt_sub(f, true)?;
                    if i < xs.len() - 1 {
                        write!(f, " * ")?;
                    }
                }
                if parens {
                    write!(f, ")")?;
                }
                Ok(())
            }
            Array(x) => write!(f, "[{}]", x),
            Var(n) => write!(f, "v{}", n),
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_sub(f, false)
    }
}
