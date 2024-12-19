use std::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

use crate::id::L;

#[derive(Clone, Debug)]
pub enum Addr {
    PreIndexed(Reg, i8),
    PostIndexed(Reg, i8),
    Offset(Reg, i8),
}

pub enum Instr {
    MovRR(Reg, Reg),
    AddImm(Reg, Reg, i8),
    Orr(Reg, Reg, Reg),
    MovImm(Reg, u16),
    Adr(Reg, String),
    Ldr(Reg, Addr),
    LdrF(FReg, Addr),
    B(L),
    Bl(L),
    Ret,
}

#[derive(Clone, Copy, Debug)]
pub enum Reg {
    /// x0, .., x30
    GPR(usize), // 0-30
    /// xzr
    ZR,
}

impl FromStr for Reg {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let regs = super::asm::regs();
        if let Some(i) = regs.iter().position(|r| r == s) {
            Ok(Reg::GPR(i))
        } else if s == "xzr" {
            Ok(Reg::ZR)
        } else {
            Err(())
        }
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Reg::GPR(i) => write!(f, "x{}", i),
            Reg::ZR => write!(f, "xzr"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FReg(usize); // 0-31

impl FromStr for FReg {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let fregs = super::asm::fregs();
        if let Some(i) = fregs.iter().position(|r| r == s) {
            Ok(FReg(i))
        } else {
            Err(())
        }
    }
}

impl Display for FReg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "d{}", self.0)
    }
}
