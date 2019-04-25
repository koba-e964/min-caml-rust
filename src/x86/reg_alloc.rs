use x86::asm;
use x86::asm::{Asm, Exp, Prog, IdOrImm};
use id::IdGen;
use syntax::Type;

use std::collections::HashMap;

type RegEnv = HashMap<String, String>;

pub fn f(Prog(data, fundefs, e): Prog, id_gen: &mut IdGen) -> Prog {
    eprintln!("register allocation: may take some time (up to a few minutes, depending on the size of functions).");
    /*
    let fundefs_p = fundefs.into_iter().map(h).collect();
    let (e_p, regenv_p) = g((id_gen.gen_tmp(&Type::Unit), Type::Unit),
                            Asm::Ans(Exp::Nop),
                            &HashMap::new(),
                            e);
    Prog(data, fundefs_p, e_p)
     */
    panic!()
}

fn g(dest: (String, Type), cont: Asm, regenv: &RegEnv, asm: Asm) -> (Asm, RegEnv) {
    use x86::asm::Asm::*;
    match asm {
        Ans(exp) => panic!(),
        _ => panic!(),
    }
}


// Internal data structures

#[derive(PartialEq, Eq, Debug)]
enum AllocResult {
    Alloc(String),
    Spill(String),
}

#[derive(PartialEq, Eq, Debug)]
struct NoReg(String, Type);

// Internal functions

fn find(x: &str, t: &Type, regenv: &RegEnv) -> Result<String, NoReg> {
    if asm::is_reg(x) { return Ok(x.to_string()); }
    match regenv.get(x) {
        Some(v) => Ok(v.clone()),
        None => Err(NoReg(x.to_string(), t.clone())),
    }
}

fn find_p(x_p: &IdOrImm, regenv: &RegEnv) -> Result<IdOrImm, NoReg> {
    match x_p {
        IdOrImm::V(ref x) => Ok(IdOrImm::V(find(x, &Type::Int, regenv)?)),
        _ => Ok(x_p.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::asm::*;
    #[test]
    fn test_find() {
        let regenv = vec![("aa".to_string(), "%eax".to_string())]
            .into_iter().collect();
        assert_eq!(find("aa", &Type::Int, &regenv), Ok("%eax".to_string()));
        assert_eq!(find("bb", &Type::Int, &regenv), Err(NoReg("bb".to_string(), Type::Int)));
    }
    #[test]
    fn test_find_p() {
        let regenv = vec![("aa".to_string(), "%eax".to_string())]
            .into_iter().collect();
        assert_eq!(find_p(&IdOrImm::V("aa".to_string()), &regenv),
                   Ok(IdOrImm::V("%eax".to_string())));
        assert_eq!(find_p(&IdOrImm::V("bb".to_string()), &regenv),
                   Err(NoReg("bb".to_string(), Type::Int)));
    }
}
