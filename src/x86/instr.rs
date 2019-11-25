use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::fmt::{Display, Formatter};

use x86::error::RegisterNameError;

const GPRS: [&str; 8] = [
    "%rax", "%rcx", "%rdx", "%rbx", "%rsp", "%rbp", "%rsi", "%rdi",
];

#[derive(Debug, Clone, Copy)]
pub struct R64(usize);

impl TryFrom<&str> for R64 {
    type Error = RegisterNameError;
    fn try_from(x: &str) -> Result<Self, Self::Error> {
        for i in 0..8 {
            if x == GPRS[i] {
                return Ok(R64(i));
            }
        }
        Err(RegisterNameError(x.to_string()))
    }
}
impl TryFrom<String> for R64 {
    type Error = RegisterNameError;
    fn try_from(x: String) -> Result<Self, Self::Error> {
        (&x as &str).try_into()
    }
}

impl Display for R64 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let &R64(index) = self;
        write!(f, "{}", GPRS[index])
    }
}

#[derive(Debug, Clone, Copy)]
pub struct M32(pub R64, pub i32);

impl Display for M32 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let &M32(reg, disp) = self;
        write!(f, "{}({})", disp, reg)
    }
}

pub enum Instr {
    // pseudo instructions
    Label(String),
    BAlign(i32),
    Globl(String),
    // instructions
    MovRR(R64, R64),
    MovIR(i32, R64),
    MovMR(M32, R64),
    Ret,
    PushQ(R64),
    PopQ(R64),
    AddIR(i32, R64),
    Call(String),
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Instr::Label(s) => writeln!(f, "{}:", s)?,
            Instr::BAlign(align) => writeln!(f, ".balign\t{}", align)?,
            Instr::Globl(s) => writeln!(f, ".globl\t{}", s)?,
            Instr::MovRR(src, dst) => writeln!(f, "    movq {}, {}", src, dst)?,
            Instr::MovIR(src, dst) => writeln!(f, "    movq ${}, {}", src, dst)?,
            Instr::MovMR(src, dst) => writeln!(f, "    movq {}, {}", src, dst)?,
            Instr::Ret => writeln!(f, "    ret")?,
            Instr::PushQ(reg) => writeln!(f, "    pushq {}", reg)?,
            Instr::PopQ(reg) => writeln!(f, "    popq {}", reg)?,
            Instr::AddIR(src, dst) => writeln!(f, "    addq ${}, {}", src, dst)?,
            Instr::Call(name) => writeln!(f, "    callq {}", name)?,
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use x86::error::Error;

    #[test]
    fn movrr_display() -> Result<(), Error> {
        let eax: R64 = TryFrom::try_from("%eax")?;
        let ebx: R64 = TryFrom::try_from("%ebx")?;
        let instr = Instr::MovRR(eax, ebx);
        let repr = format!("{}", instr);
        assert_eq!(repr, "    mov %eax, %ebx\n");
        Ok(())
    }
}
