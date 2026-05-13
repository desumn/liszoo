open StdLabels
open Liss

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
  | Assert cond -> Formula.And (cond, post)
  | Assume cond -> Formula.Imp (cond, post)

let rec vcs (cmd : Cmd.t) post =
  let open Cmd in
  match cmd.node with
  | Skip -> []
  | Assign _ -> []
  | Seq (left, right) ->
      List.concat [ vcs right post; vcs left (wp right post) ]
  | If (_, then_, else_) -> List.concat [ vcs then_ post; vcs else_ post ]
  | While (cond, inv, body) ->
      let cond_form = Conv.to_formula cond in
      List.concat
        [
          vcs body inv;
          Formula.[ Imp (And (inv, cond_form), wp body inv) ];
          Formula.[ Imp (And (inv, Not cond_form), post) ];
        ]
  | Assert _ | Assume _ -> []

let verify (prog : Program.t) =
  List.concat
    [
      Formula.[ Imp (prog.pre, wp prog.body prog.post) ];
      vcs prog.body prog.post;
    ]

let verify_and_check (prog : Program.t) =
  let open Result.Syntax in
  let vcs = verify prog in
  vcs
  |> List.map ~f:(Smt.Solver.check_alt_ergo)
  |> List.fold_left ~init:(Ok Smt.Solver.Valid)
     ~f:(fun final_result smt_result ->
       let open Smt.Solver in
       let* result = smt_result in
       let+ final_result = final_result in
       match final_result, result with
       | Valid, Valid -> Valid
       | _, _ -> Unknown `Unknown
     )
