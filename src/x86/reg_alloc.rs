use id;
use id::IdGen;
use syntax::{FloatBin, IntBin, Type};
use x86::asm;
use x86::asm::{Asm, Exp, Fundef, IdOrImm, Prog};

use std::collections::{HashMap, HashSet};

type RegEnv = HashMap<String, String>;
type Result<T> = std::result::Result<T, NoReg>;

pub fn f(Prog(data, fundefs, e): Prog, id_gen: &mut IdGen) -> Prog {
    println!("register allocation: may take some time (up to a few minutes, depending on the size of functions).");
    let fundefs_p = fundefs
        .into_vec()
        .into_iter()
        .map(|f| h(f, id_gen))
        .collect();
    let (e_p, _regenv_p) = g(
        &(id_gen.gen_tmp(&Type::Unit), Type::Unit),
        Asm::Ans(Exp::Nop),
        &HashMap::new(),
        e,
        id_gen,
    );
    Prog(data, fundefs_p, e_p)
}

// Register allocation for Fundef
// (caml2html: regalloc_h)
fn h(
    Fundef {
        name: id::L(x),
        args: ys,
        fargs: zs,
        body: e,
        ret: t,
    }: Fundef,
    id_gen: &mut IdGen,
) -> Fundef {
    let mut regenv = HashMap::new();
    regenv.insert(x.clone(), asm::reg_cl().to_string());
    let regs = asm::regs();
    let fregs = asm::fregs();
    let mut arg_regs = vec![];
    let mut farg_regs = vec![];
    for i in 0..ys.len() {
        let y = ys[i].clone();
        let r = regs[i].clone();
        arg_regs.push(r.clone());
        assert!(!asm::is_reg(&y));
        regenv.insert(y, r);
    }
    for i in 0..zs.len() {
        let z = zs[i].clone();
        let fr = fregs[i].clone();
        farg_regs.push(fr.clone());
        assert!(!asm::is_reg(&z));
        regenv.insert(z, fr);
    }
    let target = match t {
        Type::Unit => id_gen.gen_tmp(&Type::Unit),
        Type::Float => fregs[0].clone(),
        _ => regs[0].clone(),
    };
    let (e_p, _regenv_p) = g(
        &(target.clone(), t.clone()),
        Asm::Ans(Exp::Mov(target)),
        &regenv,
        e,
        id_gen,
    );
    Fundef {
        name: id::L(x),
        args: arg_regs.into_boxed_slice(),
        fargs: farg_regs.into_boxed_slice(),
        body: e_p,
        ret: t,
    }
}

fn g(
    dest: &(String, Type),
    cont: Asm,
    regenv: &RegEnv,
    asm: Asm,
    id_gen: &mut IdGen,
) -> (Asm, RegEnv) {
    use x86::asm::Asm::{Ans, Let};
    match asm {
        Ans(exp) => g_exp_with_restore(dest, cont, regenv, exp, id_gen),
        Let(x, t, exp, e) => {
            assert!(!regenv.contains_key(&x));
            let cont_p = asm::concat((*e).clone(), dest.0.clone(), dest.1.clone(), cont.clone());
            let (e1_p, regenv1) =
                g_exp_with_restore(&(x.clone(), t.clone()), cont_p.clone(), regenv, exp, id_gen);
            let (_call, targets) = target(&x, dest, &cont_p);
            let sources = source(&t, &e1_p);
            /* target first */
            let mut targets_sources = targets.clone();
            targets_sources.extend_from_slice(&sources);
            match alloc(cont_p, &regenv1, x.clone(), t.clone(), &targets_sources) {
                AllocResult::Spill(y) => {
                    let r = regenv1[&y].clone();
                    let (e2_p, regenv2) =
                        g(dest, cont, &add(x.clone(), r.clone(), regenv1), *e, id_gen);
                    let save = match regenv.get(&y) {
                        Some(var) => Exp::Save(var.clone(), y),
                        None => Exp::Nop,
                    };
                    (
                        asm::seq(id_gen, save, asm::concat(e1_p, r, t, e2_p)),
                        regenv2,
                    )
                }
                AllocResult::Alloc(reg) => {
                    let (e2_p, regenv2) = g(
                        dest,
                        cont,
                        &add(x.clone(), reg.clone(), regenv1),
                        *e,
                        id_gen,
                    );
                    (asm::concat(e1_p, reg, t, e2_p), regenv2)
                }
            }
        }
    }
}

fn g_exp_with_restore(
    dest: &(String, Type),
    cont: Asm,
    regenv: &RegEnv,
    exp: Exp,
    id_gen: &mut IdGen,
) -> (Asm, RegEnv) {
    match g_exp(dest, cont.clone(), regenv, exp.clone(), id_gen) {
        Ok(result) => result,
        Err(NoReg(x, t)) => g(
            dest,
            cont,
            regenv,
            Asm::Let(x.clone(), t, Exp::Restore(x), Box::new(Asm::Ans(exp))),
            id_gen,
        ),
    }
}

// Register allocation for Exp
fn g_exp(
    dest: &(String, Type),
    cont: Asm,
    regenv: &RegEnv,
    exp: Exp,
    id_gen: &mut IdGen,
) -> Result<(Asm, RegEnv)> {
    use self::Exp::*;
    // On branches where retval.1 != regenv, an early return is done.
    let asm = match exp {
        Nop | Set(_) | SetL(_) | Comment(_) | Restore(_) => Asm::Ans(exp),
        Mov(x) => Asm::Ans(Mov(find(&x, &Type::Int, regenv)?)),
        Neg(x) => Asm::Ans(Neg(find(&x, &Type::Int, regenv)?)),
        IntOp(op, x, y_p) => Asm::Ans(IntOp(
            op,
            find(&x, &Type::Int, regenv)?,
            find_p(&y_p, regenv)?,
        )),
        Ld(x, y_p, i) => Asm::Ans(Ld(find(&x, &Type::Int, regenv)?, find_p(&y_p, regenv)?, i)),
        St(x, y, z_p, i) => Asm::Ans(St(
            find(&x, &Type::Int, regenv)?,
            find(&y, &Type::Int, regenv)?,
            find_p(&z_p, regenv)?,
            i,
        )),
        FMovD(x) => Asm::Ans(FMovD(find(&x, &Type::Float, regenv)?)),
        FNegD(x) => Asm::Ans(FNegD(find(&x, &Type::Float, regenv)?)),
        FloatOp(op, x, y) => Asm::Ans(FloatOp(
            op,
            find(&x, &Type::Float, regenv)?,
            find(&y, &Type::Float, regenv)?,
        )),
        LdDF(x, y_p, i) => Asm::Ans(LdDF(
            find(&x, &Type::Int, regenv)?,
            find_p(&y_p, regenv)?,
            i,
        )),
        StDF(x, y, z_p, i) => Asm::Ans(StDF(
            find(&x, &Type::Float, regenv)?,
            find(&y, &Type::Int, regenv)?,
            find_p(&z_p, regenv)?,
            i,
        )),
        Save(_x, _y) => unreachable!(),
        IfComp(op, x, y_p, e1, e2) => {
            let xcp = x.clone();
            let y_pcp = y_p.clone();
            let k = |e1_p, e2_p| {
                Ok(IfComp(
                    op,
                    find(&xcp, &Type::Int, regenv)?,
                    find_p(&y_pcp, regenv)?,
                    Box::new(e1_p),
                    Box::new(e2_p),
                ))
            };
            return g_exp_if(dest, cont, regenv, k, *e1, *e2, id_gen);
        }
        IfFComp(op, x, y, e1, e2) => {
            let xcp = x.clone();
            let ycp = y.clone();
            let k = |e1_p, e2_p| {
                Ok(IfFComp(
                    op,
                    find(&xcp, &Type::Float, regenv)?,
                    find(&ycp, &Type::Float, regenv)?,
                    Box::new(e1_p),
                    Box::new(e2_p),
                ))
            };
            return g_exp_if(dest, cont, regenv, k, *e1, *e2, id_gen);
        }
        CallCls(x, ys, zs) => {
            let regs = asm::regs();
            let fregs = asm::fregs();
            if ys.len() > regs.len() - 1 || zs.len() > fregs.len() {
                panic!("cannot allocate registers for arguments to {}", x);
            }
            let k = |ys: Vec<_>, zs: Vec<_>| {
                CallCls(x.clone(), ys.into_boxed_slice(), zs.into_boxed_slice())
            };
            return g_exp_call(dest, cont, regenv, k, ys.into_vec(), zs.into_vec(), id_gen);
        }
        CallDir(id::L(x), ys, zs) => {
            let regs = asm::regs();
            let fregs = asm::fregs();
            if ys.len() > regs.len() || zs.len() > fregs.len() {
                panic!("cannot allocate registers for arguments to {}", x);
            }
            let k = |ys: Vec<_>, zs: Vec<_>| {
                CallDir(
                    id::L(x.clone()),
                    ys.into_boxed_slice(),
                    zs.into_boxed_slice(),
                )
            };
            return g_exp_call(dest, cont, regenv, k, ys.into_vec(), zs.into_vec(), id_gen);
        }
    };
    Ok((asm, regenv.clone()))
}

fn g_exp_if<F>(
    dest: &(String, Type),
    cont: Asm,
    regenv: &RegEnv,
    constr: F,
    e1: Asm,
    e2: Asm,
    id_gen: &mut IdGen,
) -> Result<(Asm, RegEnv)>
where
    F: Fn(Asm, Asm) -> Result<Exp>,
{
    let (e1_p, regenv1) = g(dest, cont.clone(), regenv, e1, id_gen);
    let (e2_p, regenv2) = g(dest, cont.clone(), regenv, e2, id_gen);
    let mut regenv_intersect = HashMap::new();
    for x in asm::fv(&cont) {
        if asm::is_reg(&x) {
            continue;
        }
        let r1 = regenv1.get(&x);
        let r2 = regenv2.get(&x);
        if let (Some(r1), Some(r2)) = (r1, r2) {
            if r1 == r2 {
                regenv_intersect.insert(x, r1.to_string());
            }
        }
    }
    let mut returned_e = Asm::Ans(constr(e1_p, e2_p)?);
    for x in asm::fv(&cont) {
        if x == dest.0 || !regenv.contains_key(&x) || regenv_intersect.contains_key(&x) {
        } else {
            returned_e = asm::seq(
                id_gen,
                Exp::Save(regenv.get(&x).unwrap().to_string(), x),
                returned_e,
            );
        }
    }
    Ok((returned_e, regenv_intersect))
}

fn g_exp_call<F>(
    dest: &(String, Type),
    cont: Asm,
    regenv: &RegEnv,
    constr: F,
    ys: Vec<String>,
    zs: Vec<String>,
    id_gen: &mut IdGen,
) -> Result<(Asm, RegEnv)>
where
    F: Fn(Vec<String>, Vec<String>) -> Exp,
{
    let empty_regenv = HashMap::new();
    // Note that Iterator<Result<A, E>> can be collected into Result<Vec<A>, E>.
    let ys = ys
        .into_iter()
        .map(|y| find(&y, &Type::Int, regenv))
        .collect::<Result<_>>()?;
    let zs = zs
        .into_iter()
        .map(|z| find(&z, &Type::Float, regenv))
        .collect::<Result<_>>()?;
    let mut e = Asm::Ans(constr(ys, zs));
    for x in asm::fv(&cont) {
        if x == dest.0 || !regenv.contains_key(&x) {
        } else {
            e = asm::seq(id_gen, Exp::Save(regenv.get(&x).unwrap().to_string(), x), e);
        }
    }
    Ok((e, empty_regenv))
}

// TODO Understand this function
// Register targeting. retval.0 is whether call is executed in asm.
fn target(src: &str, dest: &(String, Type), asm: &Asm) -> (bool, Vec<String>) {
    match asm {
        Asm::Ans(ref exp) => target_exp(src, dest, exp),
        Asm::Let(ref x, ref t, ref exp, ref e) => {
            let (c1, mut rs1) = target_exp(src, &(x.clone(), t.clone()), exp);
            if c1 {
                return (true, rs1);
            }
            let (c2, rs2) = target(src, dest, e);
            rs1.extend_from_slice(&rs2);
            (c2, rs1)
        }
    }
}

fn target_exp(src: &str, dest_t: &(String, Type), exp: &Exp) -> (bool, Vec<String>) {
    use self::Exp::*;
    let (ref dest, ref t) = dest_t;
    match exp {
        Mov(ref x) if x == src && asm::is_reg(&dest) => {
            assert_ne!(t, &Type::Unit);
            assert_ne!(t, &Type::Float);
            (false, vec![dest.clone()])
        }
        FMovD(ref x) if x == src && asm::is_reg(dest) => {
            assert_eq!(t, &Type::Float);
            (false, vec![dest.clone()])
        }
        IfComp(_, _, _, ref e1, ref e2) | IfFComp(_, _, _, ref e1, ref e2) => {
            let (c1, mut rs1) = target(src, dest_t, e1);
            let (c2, rs2) = target(src, dest_t, e2);
            rs1.extend_from_slice(&rs2);
            (c1 && c2, rs1)
        }
        CallCls(ref x, ref ys, ref zs) => {
            let mut target = target_args(src, &asm::regs(), ys);
            target.extend_from_slice(&target_args(src, &asm::fregs(), zs));
            if x == src {
                target.push(asm::reg_cl().to_string());
            }
            (true, target)
        }
        CallDir(_, ref ys, ref zs) => {
            let mut target = target_args(src, &asm::regs(), ys);
            target.extend_from_slice(&target_args(src, &asm::fregs(), zs));
            (true, target)
        }
        _ => (false, vec![]),
    }
}

fn target_args(src: &str, all: &[String], ys: &[String]) -> Vec<String> {
    let mut ans = vec![];
    assert!(ys.len() <= all.len() - 1);
    for i in 0..ys.len() {
        if ys[i] == src {
            ans.push(all[i].clone());
        }
    }
    ans
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
        Mov(ref x)
        | Neg(ref x)
        | IntOp(IntBin::Add, ref x, IdOrImm::C(_))
        | IntOp(IntBin::Sub, ref x, _)
        | FMovD(ref x)
        | FNegD(ref x)
        | FloatOp(FloatBin::FSub, ref x, _)
        | FloatOp(FloatBin::FDiv, ref x, _) => vec![x.clone()],
        IntOp(IntBin::Add, ref x, IdOrImm::V(ref y))
        | FloatOp(FloatBin::FAdd, ref x, ref y)
        | FloatOp(FloatBin::FMul, ref x, ref y) => vec![x.clone(), y.clone()],
        IfComp(_, _, _, ref e1, ref e2) => {
            let mut ans1 = source(t, e1);
            let ans2 = source(t, e2);
            ans1.extend_from_slice(&ans2);
            ans1
        }
        CallCls(..) | CallDir(..) => match t {
            Type::Unit => vec![],
            Type::Float => {
                let mut v = asm::fregs();
                v.truncate(1);
                v
            }
            _ => {
                let mut v = asm::regs();
                v.truncate(1);
                v
            }
        },
        _ => vec![],
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
    if asm::is_reg(x) {
        return Ok(x.to_string());
    }
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
fn alloc(cont: Asm, regenv: &RegEnv, x: String, t: Type, preference: &[String]) -> AllocResult {
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
    let r = preference
        .iter()
        .chain(all.iter())
        .find(|&r| !live.contains(r));
    match r {
        Some(r) => {
            println!("allocated {} to {}", x, r);
            AllocResult::Alloc(r.to_string())
        }
        None => {
            eprintln!("register allocation failed for {}.", x);
            let y = free
                .iter()
                .rev()
                .find(|&y| {
                    !asm::is_reg(y)
                        && match regenv.get(y) {
                            Some(reg) => all.contains(reg),
                            None => false,
                        }
                })
                .unwrap();
            println!("spilling {} from {}.", y, regenv[y]);
            AllocResult::Spill(y.to_string())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::asm::*;
    use super::*;
    #[test]
    fn test_find() {
        let regenv = vec![("aa".to_string(), "%rax".to_string())]
            .into_iter()
            .collect();
        assert_eq!(find("aa", &Type::Int, &regenv), Ok("%rax".to_string()));
        assert_eq!(
            find("bb", &Type::Int, &regenv),
            Err(NoReg("bb".to_string(), Type::Int))
        );
    }
    #[test]
    fn test_find_p() {
        let regenv = vec![("aa".to_string(), "%rax".to_string())]
            .into_iter()
            .collect();
        assert_eq!(
            find_p(&IdOrImm::V("aa".to_string()), &regenv),
            Ok(IdOrImm::V("%rax".to_string()))
        );
        assert_eq!(
            find_p(&IdOrImm::V("bb".to_string()), &regenv),
            Err(NoReg("bb".to_string(), Type::Int))
        );
    }
    #[test]
    fn test_alloc() {
        let cont = Asm::Ans(Exp::Nop);
        let regenv = HashMap::new();
        let x = "a".to_string();
        let t = Type::Int;
        let preference = ["%rax".to_string()];
        assert_eq!(
            alloc(cont, &regenv, x, t, &preference),
            AllocResult::Alloc("%rax".to_string()),
        )
    }
    #[test]
    fn test_alloc_not_in_preference() {
        let cont = Asm::Ans(Exp::Mov("b".to_string()));
        let regenv = vec![("b".to_string(), "%rax".to_string())]
            .into_iter()
            .collect();
        let x = "a".to_string();
        let t = Type::Int;
        let preference = ["%rax".to_string()];
        assert_eq!(
            alloc(cont, &regenv, x, t, &preference),
            AllocResult::Alloc("%rbx".to_string()),
        )
    }
}
