use x86::asm;
use x86::asm::{Asm, Exp, Prog, IdOrImm};
use id::IdGen;
use syntax::{IntBin, Type};

use std::collections::{HashMap, HashSet};

type RegEnv = HashMap<String, String>;
type Result<T> = std::result::Result<T, NoReg>;

pub fn f(Prog(data, fundefs, e): Prog, id_gen: &mut IdGen) -> Prog {
    eprintln!("register allocation: may take some time (up to a few minutes, depending on the size of functions).");
    let fundefs_p = Box::new([]);//fundefs.into_iter().map(h).collect();
    let (e_p, regenv_p) = g((id_gen.gen_tmp(&Type::Unit), Type::Unit),
                            Asm::Ans(Exp::Nop),
                            &HashMap::new(),
                            e);
    Prog(data, fundefs_p, e_p)
}

fn g(dest: (String, Type), cont: Asm, regenv: &RegEnv, asm: Asm) -> (Asm, RegEnv) {
    use x86::asm::Asm::{Ans, Let};
    match asm {
        Ans(exp) => g_exp_with_restore(dest, cont, regenv, exp),
        Let(x, y, exp, e) => {
            assert!(!regenv.contains_key(&x));
            let t = dest.1.clone();
            let cont_p = asm::concat((*e).clone(), dest.0.clone(), dest.1.clone(), cont.clone());
            let (e1_p, regenv1) = g_exp_with_restore((x.clone(), t.clone()), cont_p.clone(), regenv,
                                                     exp);
            let (_call, targets) = target(x.clone(), dest.clone(), &cont_p);
            let sources = source(&t, &e1_p);
            /* target first */
            let mut targets_sources = targets.clone();
            targets_sources.extend_from_slice(&sources);
            match alloc(cont_p, &regenv1, x.clone(), t.clone(), &targets_sources) {
                AllocResult::Spill(_) => panic!(),
                AllocResult::Alloc(reg) => {
                    let (e2_p, regenv2) =
                        g(dest, cont, &add(x.clone(), reg.clone(), regenv1), *e);
                    (asm::concat(e1_p, reg, t, e2_p), regenv2)
                }
            }
        }
    }
}

fn g_exp_with_restore(dest: (String, Type), cont: Asm, regenv: &RegEnv,
                      exp: Exp) -> (Asm, RegEnv) {
    match g_exp(dest.clone(), cont.clone(), regenv, exp.clone()) {
        Ok(result) => result,
        Err(NoReg(x, t)) =>
            g(dest, cont, regenv,
              Asm::Let(x.clone(), t, Exp::Restore(x), Box::new(Asm::Ans(exp)))),
    }
}

fn g_exp(dest: (String, Type), cont: Asm, regenv: &RegEnv,
         exp: Exp) -> Result<(Asm, RegEnv)> {
    panic!()
}

fn target(src: String, dest: (String, Type), asm: &Asm) -> (bool, Vec<String>) {
    panic!();
}

fn target_p(src: String, (dest, t): (String, Type), exp: &Exp) -> (bool, Vec<String>) {
    use self::Exp::*;
    match exp {
        Mov(ref x) if x == &src && asm::is_reg(&dest) => {
            assert_ne!(t, Type::Unit);
            assert_ne!(t, Type::Float);
            (false, vec![dest])
        },
        _ => panic!(),
    }
    /*
  | FMovD(x) when x = src && is_reg dest ->
      assert (t = Type.Float);
      false, [dest]
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfGE(_, _, e1, e2)
  | IfFEq(_, _, e1, e2) | IfFLE(_, _, e1, e2) ->
      let c1, rs1 = target src (dest, t) e1 in
      let c2, rs2 = target src (dest, t) e2 in
      c1 && c2, rs1 @ rs2
  | CallCls(x, ys, zs) ->
      true, (target_args src regs 0 ys @
             target_args src fregs 0 zs @
             if x = src then [reg_cl] else [])
  | CallDir(_, ys, zs) ->
      true, (target_args src regs 0 ys @
             target_args src fregs 0 zs)
  | _ -> false, []
     */
}

/// The author hasn't understood the meaning of this function.
/// TODO understand this
/// "register sourcing" (?) as opposed to register targeting
/// （x86の2オペランド命令のためのregister coalescing） *)
fn source(t: &Type, asm: &Asm) -> Vec<String> {
    match asm {
        Asm::Ans(ref exp) => source_exp(t, exp),
        Asm::Let(_, _, _, ref e) => source(t, e),
    }
}

fn source_exp(t: &Type, exp: &Exp) -> Vec<String> {
    use self::Exp::*;
    match exp {
        Mov(ref x) | Neg(ref x) | IntOp(IntBin::Add, ref x, IdOrImm::C(_))
            | IntOp(IntBin::Sub, ref x, _) => vec![x.clone()],
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

fn find(x: &str, t: &Type, regenv: &RegEnv) -> Result<String> {
    if asm::is_reg(x) { return Ok(x.to_string()); }
    match regenv.get(x) {
        Some(v) => Ok(v.clone()),
        None => Err(NoReg(x.to_string(), t.clone())),
    }
}

fn find_p(x_p: &IdOrImm, regenv: &RegEnv) -> Result<IdOrImm> {
    match x_p {
        IdOrImm::V(ref x) => Ok(IdOrImm::V(find(x, &Type::Int, regenv)?)),
        _ => Ok(x_p.clone()),
    }
}

fn add(x: String, reg: String, mut regenv: RegEnv) -> RegEnv {
    if asm::is_reg(&x) {
        assert_eq!(x, reg);
    } else {
        regenv.insert(x, reg);
    }
    regenv
}

// Decide to allocate a register or spill.
fn alloc(cont: Asm, regenv: &RegEnv, x: String, t: Type,
         preference: &[String]) -> AllocResult {
    assert!(!regenv.contains_key(&x));
    let all = match t {
        Type::Unit => return AllocResult::Alloc("%unit".to_string()),
        Type::Float => asm::fregs(),
        _ => asm::regs(),
    };
    if asm::is_reg(&x) {
        return AllocResult::Alloc(x);
    }
    let free: Vec<String> = asm::fv(&cont).into_iter().collect();
    // Registers that are alive
    let mut live = HashSet::new();
    for y in &free {
        if asm::is_reg(y) {
            live.insert(y.to_string());
        } else {
            if let Some(result) = regenv.get(y) {
                live.insert(result.to_string());
            }
        }
    }
    // Find a register that is not alive
    let mut r = preference.iter().chain(all.iter())
        .find(|&r| !live.contains(r));
    match r {
        Some(r) => {
            eprintln!("allocated {} to {}", x, r);
            AllocResult::Alloc(r.to_string())
        }
        None => {
            eprintln!("register allocation failed for {}.", x);
            let y = free.iter().rev()
                .find(|&y| !asm::is_reg(y) &&
                      match regenv.get(y) {
                          Some(reg) => all.contains(reg),
                          None => false,
                      }).unwrap();
            eprintln!("spilling {} from {}.", y, regenv[y]);
            AllocResult::Spill(y.to_string())
        }
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
    #[test]
    fn test_g_exp() {
    }
}
