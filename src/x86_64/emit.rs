use std::collections::HashSet;
use std::convert::TryInto;

use id;
use id::IdGen;
use syntax::IntBin;
use x86_64::asm;
use x86_64::asm::{fregs, Asm, CompBin, Exp, Fundef, IdOrImm, Prog, FREGS, REGS, REG_SP};
use x86_64::error::Error;
use x86_64::instr::Instr::MovRR;
use x86_64::instr::{addq, cmpq, loadmem, movq, savemem, subq, Instr, R64};
/*
open Asm

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

let stackset = ref S.empty (* すでにSaveされた変数の集合 (caml2html: emit_stackset) *)
    let stackmap = ref [] (* Saveされた変数の、スタックにおける位置 (caml2html: emit_stackmap) *)

 */

#[derive(Debug, Clone)]
struct StackState {
    stack_set: HashSet<String>,
    stack_map: Vec<String>,
}

impl StackState {
    fn new() -> Self {
        StackState {
            stack_set: Default::default(),
            stack_map: Vec::new(),
        }
    }
    fn save(&mut self, x: &str) {
        self.stack_set.insert(x.to_string());
        if self.stack_map.iter().all(|y| x != y) {
            self.stack_map.push(x.to_string());
        }
    }
    fn savef(&mut self, x: &str, id_gen: &mut IdGen) {
        self.stack_set.insert(x.to_string());
        if self.stack_map.iter().all(|y| x != y) {
            self.stack_map.push(x.to_string());
        }
    }

    fn offset(&self, x: &str) -> Option<i32> {
        self.stack_map
            .iter()
            .position(|y| x == y)
            .map(|y| 8 * y as i32)
    }

    fn stacksize(&self) -> i32 {
        asm::align(8 * self.stack_map.len() as i32)
    }
}

fn pp_id_or_imm(value: &IdOrImm) -> String {
    match value {
        IdOrImm::V(ref x) => x.clone(),
        IdOrImm::C(i) => format!("${}", i),
    }
}

/// 関数呼び出しのために引数を並べ替える(register shuffling) (caml2html: emit_shuffle)
fn shuffle(sw: &str, xys: &[(String, String)]) -> Vec<(String, String)> {
    let xys: Vec<_> = xys
        .iter()
        .filter(|(ref x, ref y)| x != y)
        .cloned()
        .collect();
    crate::util::assign(&xys, sw)
}
/*
let rec shuffle sw xys =
  (* remove identical moves *)
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] -> (* no acyclic moves; resolve a cyclic move *)
      (y, sw) :: (x, y) :: shuffle sw (List.map
                                         (function
                                           | (y', z) when y = y' -> (sw, z)
                                           | yz -> yz)
                                         xys)
  | xys, acyc -> acyc @ shuffle sw xys
 */

/// 末尾かどうかを表すデータ型 (caml2html: emit_dest)
#[derive(Debug)]
enum Dest {
    Tail,
    NonTail(String),
}

fn g(
    output: &mut Vec<Instr>,
    stack_state: &mut StackState,
    id_gen: &mut IdGen,
    dest: Dest,
    asm: Asm,
) -> Result<(), Error> {
    match asm {
        Asm::Ans(exp) => g_exp(output, stack_state, id_gen, dest, exp),
        Asm::Let(x, t, exp, e) => {
            g_exp(output, stack_state, id_gen, Dest::NonTail(x), exp)?;
            g(output, stack_state, id_gen, dest, *e)?;
            Ok(())
        }
    }
}
/*
let rec g oc = function (* 命令列のアセンブリ生成 (caml2html: emit_g) *)
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
      g' oc (NonTail(x), exp);
      g oc (dest, e)
 */
fn g_exp(
    output: &mut Vec<Instr>,
    stack_state: &mut StackState,
    id_gen: &mut IdGen,
    dest: Dest,
    exp: Exp,
) -> Result<(), Error> {
    use self::Dest::{NonTail, Tail};
    match (dest, exp) {
        (NonTail(x), Exp::Set(i)) => output.push(Instr::MovIR(i, x.try_into()?)),
        (NonTail(x), Exp::Mov(y)) => {
            if x != y {
                output.push(movq(IdOrImm::V(y), x)?);
            }
        }
        /*
          | NonTail(x), Add(y, z') ->
              if V(x) = z' then
                Printf.fprintf oc "\taddl\t%s, %s\n" y x
              else
                (if x <> y then Printf.fprintf oc "\tmovl\t%s, %s\n" y x;
                 Printf.fprintf oc "\taddl\t%s, %s\n" (pp_id_or_imm z') x)
        */
        (NonTail(x), Exp::IntOp(IntBin::Add, y, z_p)) => {
            if IdOrImm::V(x.clone()) == z_p {
                output.push(addq(y, x)?);
            } else {
                if x != y {
                    output.push(movq(y, x.clone())?);
                    output.push(addq(z_p, x)?);
                }
            }
        }
        /*
          | NonTail(x), Sub(y, z') ->
              if V(x) = z' then
                (Printf.fprintf oc "\tsubl\t%s, %s\n" y x;
                 Printf.fprintf oc "\tnegl\t%s\n" x)
              else
                (if x <> y then Printf.fprintf oc "\tmovl\t%s, %s\n" y x;
                 Printf.fprintf oc "\tsubl\t%s, %s\n" (pp_id_or_imm z') x)
        */
        (NonTail(x), Exp::IntOp(IntBin::Sub, y, z_p)) => {
            if IdOrImm::V(x.clone()) == z_p {
                let xreg: R64 = x.try_into()?;
                output.push(Instr::SubRR(y.try_into()?, xreg));
                output.push(Instr::NegQ(xreg));
            } else {
                let xreg = x.clone().try_into()?;
                if x != y {
                    output.push(Instr::MovRR(y.try_into()?, xreg));
                }
                output.push(subq(z_p, x)?);
            }
        }
        /*
          (* 退避の仮想命令の実装 (caml2html: emit_save) *)
          | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
              save y;
              Printf.fprintf oc "\tmovl\t%s, %d(%s)\n" x (offset y) reg_sp
          | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
              savef y;
              Printf.fprintf oc "\tmovsd\t%s, %d(%s)\n" x (offset y) reg_sp
          | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
        */
        (NonTail(_), Exp::Save(x, y)) => {
            if REGS.contains(&(x.as_ref())) && stack_state.offset(&y).is_none() {
                stack_state.save(&y);
                output.push(savemem(x, REG_SP, stack_state.offset(&y).unwrap())? );
            } else
            if FREGS.contains(&(x.as_ref())) && stack_state.offset(&y).is_none() {
                unimplemented!();
            }else {
                assert!(stack_state.offset(&y).is_some());
            }
        }
        /*
          (* 復帰の仮想命令の実装 (caml2html: emit_restore) *)
          | NonTail(x), Restore(y) when List.mem x allregs ->
              Printf.fprintf oc "\tmovl\t%d(%s), %s\n" (offset y) reg_sp x
          | NonTail(x), Restore(y) ->
              assert (List.mem x allfregs);
              Printf.fprintf oc "\tmovsd\t%d(%s), %s\n" (offset y) reg_sp x
        */
        (NonTail(x), Exp::Restore(y)) => {
            if REGS.contains(&x.as_ref()) {
                output.push(loadmem(REG_SP, stack_state.offset(&y).unwrap(), x)?);
            } else
            if FREGS.contains(&x.as_ref()) {
                unimplemented!();
            } else {
                unreachable!();
            }
        }
        /*
          | Tail, CallDir(Id.L(x), ys, zs) -> (* 末尾呼び出し *)
              g'_args oc [] ys zs;
              Printf.fprintf oc "\tjmp\t%s\n" x;
        */
        (Tail, Exp::CallDir(id::L(x), ys, zs)) => {
            // tail call
            g_exp_args(
                output,
                stack_state,
                id_gen,
                None,
                ys.into_vec(),
                zs.into_vec(),
            )?;
            output.push(Instr::Jmp(x));
        }
        /*
                  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
              g'_args oc [] ys zs;
              let ss = stacksize () in
              if ss > 0 then Printf.fprintf oc "\taddl\t$%d, %s\n" ss reg_sp;
              Printf.fprintf oc "\tcall\t%s\n" x;
              if ss > 0 then Printf.fprintf oc "\tsubl\t$%d, %s\n" ss reg_sp;
        */
        (NonTail(a), Exp::CallDir(id::L(x), ys, zs)) => {
            g_exp_args(
                output,
                stack_state,
                id_gen,
                None,
                ys.into_vec(),
                zs.into_vec(),
            )?;
            let ss = stack_state.stacksize();
            if ss > 0 {
                output.push(Instr::AddIR(ss, REG_SP.try_into()?));
            }
            output.push(Instr::Call(x));
            if ss > 0 {
                output.push(Instr::AddIR(-ss, REG_SP.try_into()?));
            }
            // TODO
            /*
              if List.mem a allregs && a <> regs.(0) then
                Printf.fprintf oc "\tmovl\t%s, %s\n" regs.(0) a
              else if List.mem a allfregs && a <> fregs.(0) then
                Printf.fprintf oc "\tmovsd\t%s, %s\n" fregs.(0) a
            */
            if REGS[0] != a {
                output.push(MovRR(REGS[0].try_into()?, a.try_into()?));
            }
        }
        /*
          | Tail, (Set _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ as exp) ->
              g' oc (NonTail(regs.(0)), exp);
              Printf.fprintf oc "\tret\n";
        */
        // Because we cannot juxtapose patterns and bind them to a different name,
        // we just duplicate patterns.
        (Tail, Exp::Mov(src)) => {
            let exp = Exp::Mov(src);
            g_exp(
                output,
                stack_state,
                id_gen,
                Dest::NonTail(REGS[0].to_string()),
                exp,
            )?;
            output.push(Instr::Ret);
        }
        (Tail, Exp::IntOp(op, x, y_p)) => {
            let exp = Exp::IntOp(op, x, y_p);
            g_exp(
                output,
                stack_state,
                id_gen,
                Dest::NonTail(REGS[0].to_string()),
                exp,
            )?;
            output.push(Instr::Ret);
        }
        (Tail, Exp::IfComp(op, x, y_p, e1, e2)) => {
            output.push(cmpq(y_p, x)?);
            let (if_label, else_label) = match op {
                CompBin::Eq => ("je", "jne"),
                CompBin::LE => ("jle", "jg"),
                CompBin::GE => ("jge", "jl"),
            };
            g_exp_tail_if(output, stack_state, id_gen, *e1, *e2, if_label, else_label)?;
        }
        (dest, exp) => unimplemented!("{:?} {:?}", dest, exp),
    }
    Ok(())
}
/*
and g' oc = function (* 各命令のアセンブリ生成 (caml2html: emit_gprime) *)
  (* 末尾でなかったら計算結果をdestにセット (caml2html: emit_nontail) *)
  | NonTail(_), Nop -> ()
  | NonTail(x), Set(i) -> Printf.fprintf oc "\tmovl\t$%d, %s\n" i x
  | NonTail(x), SetL(Id.L(y)) -> Printf.fprintf oc "\tmovl\t$%s, %s\n" y x
  | NonTail(x), Mov(y) ->
      if x <> y then Printf.fprintf oc "\tmovl\t%s, %s\n" y x
  | NonTail(x), Neg(y) ->
      if x <> y then Printf.fprintf oc "\tmovl\t%s, %s\n" y x;
      Printf.fprintf oc "\tnegl\t%s\n" x
  | NonTail(x), Add(y, z') ->
      if V(x) = z' then
        Printf.fprintf oc "\taddl\t%s, %s\n" y x
      else
        (if x <> y then Printf.fprintf oc "\tmovl\t%s, %s\n" y x;
         Printf.fprintf oc "\taddl\t%s, %s\n" (pp_id_or_imm z') x)
  | NonTail(x), Sub(y, z') ->
      if V(x) = z' then
        (Printf.fprintf oc "\tsubl\t%s, %s\n" y x;
         Printf.fprintf oc "\tnegl\t%s\n" x)
      else
        (if x <> y then Printf.fprintf oc "\tmovl\t%s, %s\n" y x;
         Printf.fprintf oc "\tsubl\t%s, %s\n" (pp_id_or_imm z') x)
  | NonTail(x), Ld(y, V(z), i) -> Printf.fprintf oc "\tmovl\t(%s,%s,%d), %s\n" y z i x
  | NonTail(x), Ld(y, C(j), i) -> Printf.fprintf oc "\tmovl\t%d(%s), %s\n" (j * i) y x
  | NonTail(_), St(x, y, V(z), i) -> Printf.fprintf oc "\tmovl\t%s, (%s,%s,%d)\n" x y z i
  | NonTail(_), St(x, y, C(j), i) -> Printf.fprintf oc "\tmovl\t%s, %d(%s)\n" x (j * i) y
  | NonTail(x), FMovD(y) ->
      if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x
  | NonTail(x), FNegD(y) ->
      if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
      Printf.fprintf oc "\txorpd\tmin_caml_fnegd, %s\n" x
  | NonTail(x), FAddD(y, z) ->
      if x = z then
        Printf.fprintf oc "\taddsd\t%s, %s\n" y x
      else
        (if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
         Printf.fprintf oc "\taddsd\t%s, %s\n" z x)
  | NonTail(x), FSubD(y, z) ->
      if x = z then (* [XXX] ugly *)
        let ss = stacksize () in
        Printf.fprintf oc "\tmovsd\t%s, %d(%s)\n" z ss reg_sp;
        if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
        Printf.fprintf oc "\tsubsd\t%d(%s), %s\n" ss reg_sp x
      else
        (if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
         Printf.fprintf oc "\tsubsd\t%s, %s\n" z x)
  | NonTail(x), FMulD(y, z) ->
      if x = z then
        Printf.fprintf oc "\tmulsd\t%s, %s\n" y x
      else
        (if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
         Printf.fprintf oc "\tmulsd\t%s, %s\n" z x)
  | NonTail(x), FDivD(y, z) ->
      if x = z then (* [XXX] ugly *)
        let ss = stacksize () in
        Printf.fprintf oc "\tmovsd\t%s, %d(%s)\n" z ss reg_sp;
        if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
        Printf.fprintf oc "\tdivsd\t%d(%s), %s\n" ss reg_sp x
      else
        (if x <> y then Printf.fprintf oc "\tmovsd\t%s, %s\n" y x;
         Printf.fprintf oc "\tdivsd\t%s, %s\n" z x)
  | NonTail(x), LdDF(y, V(z), i) -> Printf.fprintf oc "\tmovsd\t(%s,%s,%d), %s\n" y z i x
  | NonTail(x), LdDF(y, C(j), i) -> Printf.fprintf oc "\tmovsd\t%d(%s), %s\n" (j * i) y x
  | NonTail(_), StDF(x, y, V(z), i) -> Printf.fprintf oc "\tmovsd\t%s, (%s,%s,%d)\n" x y z i
  | NonTail(_), StDF(x, y, C(j), i) -> Printf.fprintf oc "\tmovsd\t%s, %d(%s)\n" x (j * i) y
  | NonTail(_), Comment(s) -> Printf.fprintf oc "\t# %s\n" s
  (* 退避の仮想命令の実装 (caml2html: emit_save) *)
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
      Printf.fprintf oc "\tmovl\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
      savef y;
      Printf.fprintf oc "\tmovsd\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
  (* 復帰の仮想命令の実装 (caml2html: emit_restore) *)
  | NonTail(x), Restore(y) when List.mem x allregs ->
      Printf.fprintf oc "\tmovl\t%d(%s), %s\n" (offset y) reg_sp x
  | NonTail(x), Restore(y) ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "\tmovsd\t%d(%s), %s\n" (offset y) reg_sp x
  (* 末尾だったら計算結果を第一レジスタにセットしてret (caml2html: emit_tailret) *)
  | Tail, (Nop | St _ | StDF _ | Comment _ | Save _ as exp) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\tret\n";
  | Tail, (Set _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ as exp) ->
      g' oc (NonTail(regs.(0)), exp);
      Printf.fprintf oc "\tret\n";
  | Tail, (FMovD _ | FNegD _ | FAddD _ | FSubD _ | FMulD _ | FDivD _ | LdDF _  as exp) ->
      g' oc (NonTail(fregs.(0)), exp);
      Printf.fprintf oc "\tret\n";
  | Tail, (Restore(x) as exp) ->
      (match locate x with
      | [i] -> g' oc (NonTail(regs.(0)), exp)
      | [i; j] when i + 1 = j -> g' oc (NonTail(fregs.(0)), exp)
      | _ -> assert false);
      Printf.fprintf oc "\tret\n";
  | Tail, IfEq(x, y', e1, e2) ->
      Printf.fprintf oc "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x;
      g'_tail_if oc e1 e2 "je" "jne"
  | Tail, IfLE(x, y', e1, e2) ->
      Printf.fprintf oc "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x;
      g'_tail_if oc e1 e2 "jle" "jg"
  | Tail, IfGE(x, y', e1, e2) ->
      Printf.fprintf oc "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x;
      g'_tail_if oc e1 e2 "jge" "jl"
  | Tail, IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tcomisd\t%s, %s\n" y x;
      g'_tail_if oc e1 e2 "je" "jne"
  | Tail, IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tcomisd\t%s, %s\n" y x;
      g'_tail_if oc e1 e2 "jbe" "ja"
  | NonTail(z), IfEq(x, y', e1, e2) ->
      Printf.fprintf oc "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "je" "jne"
  | NonTail(z), IfLE(x, y', e1, e2) ->
      Printf.fprintf oc "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "jle" "jg"
  | NonTail(z), IfGE(x, y', e1, e2) ->
      Printf.fprintf oc "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "jge" "jl"
  | NonTail(z), IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tcomisd\t%s, %s\n" y x;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "je" "jne"
  | NonTail(z), IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tcomisd\t%s, %s\n" y x;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "jbe" "ja"
  (* 関数呼び出しの仮想命令の実装 (caml2html: emit_call) *)
  | Tail, CallCls(x, ys, zs) -> (* 末尾呼び出し (caml2html: emit_tailcall) *)
      g'_args oc [(x, reg_cl)] ys zs;
      Printf.fprintf oc "\tjmp\t*(%s)\n" reg_cl;
  | Tail, CallDir(Id.L(x), ys, zs) -> (* 末尾呼び出し *)
      g'_args oc [] ys zs;
      Printf.fprintf oc "\tjmp\t%s\n" x;
  | NonTail(a), CallCls(x, ys, zs) ->
      g'_args oc [(x, reg_cl)] ys zs;
      let ss = stacksize () in
      if ss > 0 then Printf.fprintf oc "\taddl\t$%d, %s\n" ss reg_sp;
      Printf.fprintf oc "\tcall\t*(%s)\n" reg_cl;
      if ss > 0 then Printf.fprintf oc "\tsubl\t$%d, %s\n" ss reg_sp;
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\tmovl\t%s, %s\n" regs.(0) a
      else if List.mem a allfregs && a <> fregs.(0) then
        Printf.fprintf oc "\tmovsd\t%s, %s\n" fregs.(0) a
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
      g'_args oc [] ys zs;
      let ss = stacksize () in
      if ss > 0 then Printf.fprintf oc "\taddl\t$%d, %s\n" ss reg_sp;
      Printf.fprintf oc "\tcall\t%s\n" x;
      if ss > 0 then Printf.fprintf oc "\tsubl\t$%d, %s\n" ss reg_sp;
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\tmovl\t%s, %s\n" regs.(0) a
      else if List.mem a allfregs && a <> fregs.(0) then
        Printf.fprintf oc "\tmovsd\t%s, %s\n" fregs.(0) a
 */

fn g_exp_tail_if(
    output: &mut Vec<Instr>,
    stack_state: &mut StackState,
    id_gen: &mut IdGen,
    e1: Asm,
    e2: Asm,
    b: &str,
    bn: &str,
) -> Result<(), Error> {
    let stack_state_old = stack_state.clone();
    let b_else = id_gen.gen_id(&format!("{}_else", b));
    output.push(Instr::Branch(bn.to_string(), b_else.clone()));
    g(output, stack_state, id_gen, Dest::Tail, e1)?;
    output.push(Instr::Label(b_else));
    *stack_state = stack_state_old;
    g(output, stack_state, id_gen, Dest::Tail, e2)?;
    Ok(())
}
/*
and g'_tail_if oc e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  Printf.fprintf oc "\t%s\t%s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)
*/
fn g_exp_non_tail_if(
    output: &mut Vec<Instr>,
    stack_state: &mut StackState,
    id_gen: &mut IdGen,
    dest: Dest,
    e1: Exp,
    e2: Exp,
    b: &str,
    bn: &str,
) -> Result<(), Error> {
    unimplemented!("e1={:?}, e2={:?}, b={}, bn={}", e1, e2, b, bn);
}
/*
and g'_non_tail_if oc dest e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.fprintf oc "\t%s\t%s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (dest, e1);
  let stackset1 = !stackset in
  Printf.fprintf oc "\tjmp\t%s\n" b_cont;
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (dest, e2);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
 */
fn g_exp_args(
    output: &mut Vec<Instr>,
    stack_state: &mut StackState,
    id_gen: &mut IdGen,
    x_reg_cl: Option<String>,
    ys: Vec<String>,
    zs: Vec<String>,
) -> Result<(), Error> {
    assert!(ys.len() <= REGS.len() - if x_reg_cl.is_some() { 1 } else { 0 });
    assert!(zs.len() <= fregs().len());
    let sw = "%r15"; // TODO register to instr
    let mut swaps = vec![];
    for i in 0..ys.len() {
        swaps.push((ys[i].clone(), REGS[i].to_string()));
        // TODO handle x_reg_cl
    }
    // shuffle
    let swaps = shuffle(&sw, &swaps);
    for i in 0..swaps.len() {
        let (x, y) = swaps[i].clone();
        output.push(Instr::MovRR(x.try_into()?, y.try_into()?));
    }
    // TODO floating point numbers
    Ok(())
}
/*
and g'_args oc x_reg_cl ys zs =
  assert (List.length ys <= Array.length regs - List.length x_reg_cl);
  assert (List.length zs <= Array.length fregs);
  let sw = Printf.sprintf "%d(%s)" (stacksize ()) reg_sp in
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> Printf.fprintf oc "\tmovl\t%s, %s\n" y r)
    (shuffle sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (fun (z, fr) -> Printf.fprintf oc "\tmovsd\t%s, %s\n" z fr)
    (shuffle sw zfrs)
 */

fn h(
    output: &mut Vec<Instr>,
    Fundef {
        name: id::L(x),
        args: ys,
        fargs: zs,
        body: e,
        ret: t,
    }: Fundef,
    id_gen: &mut IdGen,
) -> Result<(), Error> {
    output.push(Instr::Label(x));
    g(output, &mut StackState::new(), id_gen, Dest::Tail, e)
}
/*
let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)
*/

pub fn f(Prog(data, fundefs, e): Prog, id_gen: &mut IdGen) -> Result<Vec<Instr>, Error> {
    let mut instrs = Vec::new();
    instrs.push(Instr::Section(".data".to_string()));
    instrs.push(Instr::BAlign(8));
    for (id::L(x), d) in data.into_vec() {
        // TODO
    }
    instrs.push(Instr::Section(".text".to_string()));
    for fundef in fundefs.into_vec() {
        h(&mut instrs, fundef, id_gen)?;
    }
    instrs.push(Instr::Globl("min_caml_start".to_string()));
    instrs.push(Instr::Label("min_caml_start".to_string()));
    instrs.push(Instr::Globl("_min_caml_start".to_string()));
    instrs.push(Instr::Label("_min_caml_start".to_string()));
    let saved_regs = vec!["%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "%rbp"];
    for reg in saved_regs.iter().cloned() {
        instrs.push(Instr::PushQ(reg.try_into()?))
    }
    /*
          Printf.fprintf oc "\tmovl\t32(%%esp),%s\n" reg_sp;
      Printf.fprintf oc "\tmovl\t36(%%esp),%s\n" regs.(0);
      Printf.fprintf oc "\tmovl\t%s,%s\n" regs.(0) reg_hp;
    */
    instrs.push(movq("%rdi".to_string(), REG_SP)?); // %rdi = sp
                                                    // TODO: %rsi = hp
    g(
        &mut instrs,
        &mut StackState::new(),
        id_gen,
        Dest::NonTail(saved_regs[0].to_string()),
        e,
    )?;
    for reg in saved_regs.into_iter().rev() {
        instrs.push(Instr::PopQ(reg.try_into()?))
    }
    instrs.push(Instr::Ret);
    Ok(instrs)
}
/*
let f oc (Prog(data, fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc ".data\n";
  Printf.fprintf oc ".balign\t8\n";
  List.iter
    (fun (Id.L(x), d) ->
      Printf.fprintf oc "%s:\t# %f\n" x d;
      Printf.fprintf oc "\t.long\t0x%lx\n" (gethi d);
      Printf.fprintf oc "\t.long\t0x%lx\n" (getlo d))
    data;
  Printf.fprintf oc ".text\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc ".globl\tmin_caml_start\n";
  Printf.fprintf oc "min_caml_start:\n";
  Printf.fprintf oc ".globl\t_min_caml_start\n";
  Printf.fprintf oc "_min_caml_start: # for cygwin\n";
  Printf.fprintf oc "\tpushl\t%%eax\n";
  Printf.fprintf oc "\tpushl\t%%ebx\n";
  Printf.fprintf oc "\tpushl\t%%ecx\n";
  Printf.fprintf oc "\tpushl\t%%edx\n";
  Printf.fprintf oc "\tpushl\t%%esi\n";
  Printf.fprintf oc "\tpushl\t%%edi\n";
  Printf.fprintf oc "\tpushl\t%%ebp\n";
  Printf.fprintf oc "\tmovl\t32(%%esp),%s\n" reg_sp;
  Printf.fprintf oc "\tmovl\t36(%%esp),%s\n" regs.(0);
  Printf.fprintf oc "\tmovl\t%s,%s\n" regs.(0) reg_hp;
  stackset := S.empty;
  stackmap := [];
  g oc (NonTail(regs.(0)), e);
  Printf.fprintf oc "\tpopl\t%%ebp\n";
  Printf.fprintf oc "\tpopl\t%%edi\n";
  Printf.fprintf oc "\tpopl\t%%esi\n";
  Printf.fprintf oc "\tpopl\t%%edx\n";
  Printf.fprintf oc "\tpopl\t%%ecx\n";
  Printf.fprintf oc "\tpopl\t%%ebx\n";
  Printf.fprintf oc "\tpopl\t%%eax\n";
  Printf.fprintf oc "\tret\n";
*/
