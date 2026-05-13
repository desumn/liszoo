open StdLabels
open Liss

type kind =
  | Pre_implies_wp
  | Loop_invariant_preserved
  | Loop_exit
  | Assertion

type vc = {
  kind : kind;
  loc : Lexing.position * Lexing.position;
  goal : Formula.t
}

let rec wp (cmd : Cmd.t) post =
  let open Cmd in
  match cmd.node with
  | Skip -> post
  | Assign (name, expr) -> Formula.subst name (Conv.to_term expr) post
  | Seq (left, right) -> wp left (wp right post)
  | If (cond, then_, else_) ->
      let cond_form = Conv.to_formula cond in
      Formula.(
        And (Imp (cond_form, wp then_ post), Imp (Not cond_form, wp else_ post)))
  | While (_, inv, _) -> inv
  | Assert cond -> Formula.Imp (cond, post)
  | Assume cond -> Formula.Imp (cond, post)

let rec vcs (cmd : Cmd.t) post =
  let open Cmd in
  match cmd.node with
  | Skip -> []
  | Assign _ -> []
  | Seq (left, right) ->
      let vcs_right = vcs right post in
      let vcs_left = vcs left (wp right post) in
      let vcs_right = List.map vcs_right ~f:(fun vc -> {vc with goal = wp left vc.goal}) in
      List.concat [ vcs_left; vcs_right ]
  | If (cond, then_, else_) ->
    let cond_form = Conv.to_formula cond in
    let vcs_then = List.map (vcs then_ post) ~f:(fun vc -> {vc with goal = Imp (cond_form, vc.goal)}) in
    let vcs_else = List.map (vcs else_ post) ~f:(fun vc -> {vc with goal = Imp (cond_form, vc.goal)}) in
    List.concat [ vcs_then; vcs_else ]
  | While (cond, inv, body) ->
      let loc = body.loc in
      let cond_form = Conv.to_formula cond in
      let body_vcs = List.map (vcs body inv) ~f:(fun vc -> {vc with goal = Imp (And(inv, cond_form), vc.goal)}) in
      List.concat
        [
          body_vcs;
          [{ loc; kind = Loop_invariant_preserved ; goal = Formula.( Imp (And (inv, cond_form), wp body inv) )}];
          [{ loc; kind = Loop_exit; goal = Formula.(Imp (And (inv, Not cond_form), post))}];
        ]
  | Assert cond -> [{loc = cmd.loc; kind = Assertion; goal = cond}]
  | Assume _ -> []

let verify (prog : Program.t) =
  List.concat
    [
      [{ loc = Lexing.dummy_pos, Lexing.dummy_pos;
        kind = Pre_implies_wp;
        goal = Formula.(Imp (prog.pre, wp prog.body prog.post)) }];
      vcs prog.body prog.post |> List.map ~f:(fun vc -> {vc with goal = Imp (prog.pre, vc.goal)});
    ]

let verify_and_check (prog : Program.t) =
  let vcs = verify prog in
  vcs
  |> List.map ~f:(fun vc -> vc, Smt.Solver.check_alt_ergo vc.goal)
