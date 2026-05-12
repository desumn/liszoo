type t =
  | Var of string
  | Const of int
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t

let rec free_vars = function
  | Var var -> Common.StringSet.singleton var
  | Const _ -> Common.StringSet.empty
  | Neg term -> free_vars term
  | Add (l, r) | Sub (l, r) | Mul (l, r) ->
      Common.StringSet.union (free_vars l) (free_vars r)

let rec subst var term subst_term =
  match term with
  | Var target when var = target -> subst_term
  | Var target -> Var target
  | Const c -> Const c
  | Neg t -> Neg (subst var t subst_term)
  | Add (l, r) -> Add (subst var l subst_term, subst var r subst_term)
  | Sub (l, r) -> Sub (subst var l subst_term, subst var r subst_term)
  | Mul (l, r) -> Mul (subst var l subst_term, subst var r subst_term)

let add_level = 1
let sub_level = 1
let mul_level = 2
let neg_level = 3

let rec pp_at level ppf term =
  let open Common.Pretty in
  match term with
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
let equal (term1 : t) (term2 : t) = term1 = term2
let compare (term1 : t) (term2 : t) = compare term1 term2
