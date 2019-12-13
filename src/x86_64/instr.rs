use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::fmt::{Display, Formatter};

use x86_64::asm::IdOrImm;
use x86_64::error::{Error, RegisterNameError};

const GPRS: [&str; 16] = [
    "%rax", "%rcx", "%rdx", "%rbx", "%rsp", "%rbp", "%rsi", "%rdi", "%r8", "%r9", "%r10", "%r11",
    "%r12", "%r13", "%r14", "%r15",
];

#[derive(Debug, Clone, Copy)]
pub struct R64(usize);

impl TryFrom<&str> for R64 {
    type Error = RegisterNameError;
    fn try_from(x: &str) -> Result<Self, Self::Error> {
        for (i, &name) in GPRS.iter().enumerate() {
            if x == name {
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
pub enum RI64 {
    R64(R64),
    Imm(i32),
}

impl TryFrom<IdOrImm> for RI64 {
    type Error = RegisterNameError;
    fn try_from(x: IdOrImm) -> Result<Self, Self::Error> {
        let returned = match x {
            IdOrImm::C(value) => RI64::Imm(value),
            IdOrImm::V(name) => RI64::R64(name.try_into()?),
        };
        Ok(returned)
    }
}

impl TryFrom<String> for RI64 {
    type Error = RegisterNameError;
    fn try_from(x: String) -> Result<Self, Self::Error> {
        Ok(RI64::R64(x.try_into()?))
    }
}

impl Display for RI64 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            RI64::R64(reg) => write!(f, "{}", reg),
            RI64::Imm(value) => write!(f, "${}", value),
        }
    }
}

// displacement
pub type Disp = i32;

#[derive(Debug, Clone, Copy)]
pub struct M32(pub R64, pub Disp);

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
    Section(String),
    // instructions
    MovRR(R64, R64),
    MovIR(i32, R64),
    MovMR(M32, R64),
    MovRM(R64, M32),
    Ret,
    PushQ(R64),
    PopQ(R64),
    AddRR(R64, R64),
    AddIR(i32, R64),
    SubRR(R64, R64),
    SubIR(i32, R64),
    NegQ(R64),
    CmpQ(RI64, R64),
    Branch(String, String),
    Jmp(String),
    Call(String),
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Instr::Label(s) => writeln!(f, "{}:", s)?,
            Instr::BAlign(align) => writeln!(f, ".balign\t{}", align)?,
            Instr::Globl(s) => writeln!(f, ".globl\t{}", s)?,
            Instr::Section(name) => writeln!(f, "{}", name)?,
            Instr::MovRR(src, dst) => writeln!(f, "    movq {}, {}", src, dst)?,
            Instr::MovIR(src, dst) => writeln!(f, "    movq ${}, {}", src, dst)?,
            Instr::MovMR(src, dst) => writeln!(f, "    movq {}, {}", src, dst)?,
            Instr::MovRM(src, dst) => writeln!(f, "    movq {}, {}", src, dst)?,
            Instr::Ret => writeln!(f, "    ret")?,
            Instr::PushQ(reg) => writeln!(f, "    pushq {}", reg)?,
            Instr::PopQ(reg) => writeln!(f, "    popq {}", reg)?,
            Instr::AddRR(src, dst) => writeln!(f, "    addq {}, {}", src, dst)?,
            Instr::AddIR(src, dst) => writeln!(f, "    addq ${}, {}", src, dst)?,
            Instr::SubRR(src, dst) => writeln!(f, "    subq {}, {}", src, dst)?,
            Instr::SubIR(src, dst) => writeln!(f, "    subq ${}, {}", src, dst)?,
            Instr::NegQ(reg) => writeln!(f, "    negq {}", reg)?,
            Instr::CmpQ(src, dst) => writeln!(f, "    cmpq {}, {}", src, dst)?,
            Instr::Branch(op, to) => writeln!(f, "    {} {}", op, to)?,
            Instr::Jmp(to) => writeln!(f, "    jmp {}", to)?,
            Instr::Call(name) => writeln!(f, "    callq {}", name)?,
        }
        Ok(())
    }
}

// helper functions for instructions
pub fn movq(
    src: impl TryInto<RI64, Error = RegisterNameError>,
    dst: impl TryInto<R64, Error = RegisterNameError>,
) -> Result<Instr, RegisterNameError> {
    let instr = match src.try_into()? {
        RI64::Imm(value) => Instr::MovIR(value, dst.try_into()?),
        RI64::R64(r) => Instr::MovRR(r, dst.try_into()?),
    };
    Ok(instr)
}

pub fn savemem(
    src: impl TryInto<R64, Error = RegisterNameError>,
    dstreg: impl TryInto<R64, Error = RegisterNameError>,
    dstoffset: Disp,
) -> Result<Instr, RegisterNameError> {
    let src = src.try_into()?;
    let dstreg = dstreg.try_into()?;
    let instr = Instr::MovRM(src, M32(dstreg, dstoffset));
    Ok(instr)
}

pub fn loadmem(
    srcreg: impl TryInto<R64, Error = RegisterNameError>,
    srcoffset: Disp,
    dst: impl TryInto<R64, Error = RegisterNameError>,
) -> Result<Instr, RegisterNameError> {
    let srcreg = srcreg.try_into()?;
    let dst = dst.try_into()?;
    let instr = Instr::MovMR(M32(srcreg, srcoffset), dst);
    Ok(instr)
}

pub fn addq(
    src: impl TryInto<RI64, Error = RegisterNameError>,
    dst: impl TryInto<R64, Error = RegisterNameError>,
) -> Result<Instr, Error> {
    let instr = match src.try_into()? {
        RI64::Imm(value) => Instr::AddIR(value, dst.try_into()?),
        RI64::R64(r) => Instr::AddRR(r, dst.try_into()?),
    };
    Ok(instr)
}
pub fn subq(
    src: impl TryInto<RI64, Error = RegisterNameError>,
    dst: impl TryInto<R64, Error = RegisterNameError>,
) -> Result<Instr, Error> {
    let instr = match src.try_into()? {
        RI64::Imm(value) => Instr::SubIR(value, dst.try_into()?),
        RI64::R64(r) => Instr::SubRR(r, dst.try_into()?),
    };
    Ok(instr)
}
pub fn cmpq(
    src: impl TryInto<RI64, Error = RegisterNameError>,
    dst: impl TryInto<R64, Error = RegisterNameError>,
) -> Result<Instr, Error> {
    Ok(Instr::CmpQ(src.try_into()?, dst.try_into()?))
}

#[cfg(test)]
mod tests {
    use super::*;
    use x86_64::error::Error;

    #[test]
    fn movrr_display() -> Result<(), Error> {
        let rax: R64 = TryFrom::try_from("%rax")?;
        let rbx: R64 = TryFrom::try_from("%rbx")?;
        let instr = Instr::MovRR(rax, rbx);
        let repr = format!("{}", instr);
        assert_eq!(repr, "    movq %rax, %rbx\n");
        Ok(())
    }
}
