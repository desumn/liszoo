type t =
  | Var of string
  | Const of int
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t

let rec free_vars expr =
  match expr with
  | Var var -> Common.StringSet.singleton var
  | Const _ -> Common.StringSet.empty
  | Neg expr -> free_vars expr
  | Add (l, r) | Sub (l, r) | Mul (l, r) ->
      Common.StringSet.union (free_vars l) (free_vars r)

let equal (expr1 : t) (expr2 : t) = expr1 = expr2
let compare (expr1 : t) (expr2 : t) = compare expr1 expr2
let add_level = 1
let sub_level = 1
let mul_level = 2
let neg_level = 3

let rec pp_at level ppf expr =
  let open Common.Pretty in
  match expr with
  | Var var -> Fmt.string ppf var
  | Const const -> Fmt.int ppf const
  | Neg term ->
      paren_if (level > neg_level) ppf @@ fun () ->
      Fmt.pf ppf "-%a" (pp_at (neg_level + 1)) term
  | Add (l, r) ->
      paren_if (level > add_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a +@ %a@]" (pp_at add_level) l (pp_at (add_level + 1)) r
  | Sub (l, r) ->
      paren_if (level > sub_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a -@ %a@]" (pp_at sub_level) l (pp_at (sub_level + 1)) r
  | Mul (l, r) ->
      paren_if (level > mul_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a *@ %a@]" (pp_at mul_level) l (pp_at (mul_level + 1)) r

let pp = pp_at 0
