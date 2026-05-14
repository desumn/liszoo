
let rec unroll_cmd depth (cmd : Liss.Cmd.t) = 
  let open Liss.Cmd in
  match cmd.node with
  | Skip |  Assign _ -> cmd
  | Seq (l, r) -> { cmd with node = Seq (unroll_cmd depth l, unroll_cmd depth r)}
  | If (cond, then_, else_) -> { cmd with node = If (cond, unroll_cmd depth then_, unroll_cmd depth else_) }
  | While (cond, _, body) ->
    let unrolled_body = unroll_cmd depth body in
    let rec loop k =
    match k with
    | 0 -> Assume (Formula.Bot)
    | k -> If (cond, Common.Located.dummy @@ Seq (unrolled_body, Common.Located.dummy @@ loop (k - 1)), Common.Located.dummy Skip)
    in
    { cmd with node = loop depth }
  | Assert _ | Assume _ -> cmd
