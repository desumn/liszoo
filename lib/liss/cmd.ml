type node =
  | Skip
  | Assign of string * Expr.t
  | Seq    of t * t
  | If     of Bool_expr.t * t * t
  | While  of Bool_expr.t * Formula.t * t
  | Assert of Formula.t
  | Assume of Formula.t
and t = node Common.Located.t

let rec equal (cmd1 : t) (cmd2 : t) =
  match cmd1.node, cmd2.node with
  | Skip, Skip -> true
  | Assign (to1, expr1), Assign (to2, expr2)
      -> String.equal to1 to2 && Expr.equal expr1 expr2
  | Assert cond1, Assert cond2 -> Formula.equal cond1 cond2
  | Assume cond1, Assume cond2 -> Formula.equal cond1 cond2
  | Seq (left1, right1), Seq (left2, right2) -> equal left1 left2 && equal right1 right2
  | If (cond1, then1, else1), If (cond2, then2, else2)
      -> Bool_expr.equal cond1 cond2 && equal then1 then2 && equal else1 else2
  | While (cond1, inv1, body1), While (cond2, inv2, body2)
      -> Bool_expr.equal cond1 cond2 && Formula.equal inv1 inv2 && equal body1 body2
| ( Skip | Assign _ | Seq _ | If _ | While _ | Assert _ | Assume _ ), _ ->
    false

let rec pp ppf cmd = Fmt.pf ppf "@[<v>%a@]" pp_node cmd

and pp_node ppf (cmd : t) =
  match cmd.node with
  | Skip -> Fmt.string ppf "skip"
  | Assign (x, e) -> Fmt.pf ppf "%s := %a" x Expr.pp e
  | Assert f -> Fmt.pf ppf "assert { %a }" Formula.pp f
  | Assume f -> Fmt.pf ppf "assume { %a }" Formula.pp f
  | Seq (s1, s2) ->
      Fmt.pf ppf "%a;@,%a" pp_node s1 pp_node s2
  | If (c, t, e) ->
      Fmt.pf ppf
        "@[<v 2>if %a then@,%a@]@,@[<v 2>else@,%a@]@,end"
        Bool_expr.pp c pp_node t pp_node e
  | While (c, i, b) ->
      Fmt.pf ppf
        "@[<v 2>while %a invariant { %a } do@,%a@]@,end"
        Bool_expr.pp c Formula.pp i pp_node b
