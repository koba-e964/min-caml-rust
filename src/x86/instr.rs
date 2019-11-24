use std::convert::TryFrom;
use std::fmt;
use std::fmt::{Display, Formatter};

use x86::error::RegisterNameError;

const GPRS: [&str; 8] = ["eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi"];

#[derive(Debug, Clone, Copy)]
pub struct R32(usize);

impl TryFrom<&str> for R32 {
    type Error = RegisterNameError;
    fn try_from(x: &str) -> Result<Self, Self::Error> {
        for i in 0..8 {
            if x == GPRS[i] {
                return Ok(R32(i));
            }
        }
        Err(RegisterNameError)
    }
}

impl Display for R32 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let &R32(index) = self;
        write!(f, "%{}", GPRS[index])
    }
}

pub enum Instr {
    MovRR(R32, R32),
    Ret,
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Instr::MovRR(src, dst) => write!(f, "mov {}, {}\n", src, dst)?,
            Instr::Ret => write!(f, "ret\n")?,
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
        let eax: R32 = TryFrom::try_from("eax")?;
        let ebx: R32 = TryFrom::try_from("ebx")?;
        let instr = Instr::MovRR(eax, ebx);
        let repr = format!("{}", instr);
        assert_eq!(repr, "mov %eax, %ebx\n");
        Ok(())
    }
}
