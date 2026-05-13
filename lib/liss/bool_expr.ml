type atom =
  | Eq of Expr.t * Expr.t
  | Neq of Expr.t * Expr.t
  | Lt of Expr.t * Expr.t
  | Le of Expr.t * Expr.t
  | Gt of Expr.t * Expr.t
  | Ge of Expr.t * Expr.t

let pp_atom ppf atom =
  match atom with
  | Eq (l, r) -> Fmt.pf ppf "%a = %a" Expr.pp l Expr.pp r
  | Neq (l, r) -> Fmt.pf ppf "%a /= %a" Expr.pp l Expr.pp r
  | Lt (l, r) -> Fmt.pf ppf "%a < %a" Expr.pp l Expr.pp r
  | Le (l, r) -> Fmt.pf ppf "%a <= %a" Expr.pp l Expr.pp r
  | Gt (l, r) -> Fmt.pf ppf "%a > %a" Expr.pp l Expr.pp r
  | Ge (l, r) -> Fmt.pf ppf "%a >= %a" Expr.pp l Expr.pp r

type t = True | False | Atom of atom | Not of t | And of t * t | Or of t * t

let rec free_vars bool_expr =
  match bool_expr with
  | True | False -> Common.StringSet.empty
  | Atom (Eq (l, r) | Neq (l, r) | Lt (l, r) | Le (l, r) | Gt (l, r) | Ge (l, r))
    ->
      Common.StringSet.union (Expr.free_vars l) (Expr.free_vars r)
  | Not b -> free_vars b
  | And (l, r) | Or (l, r) -> Common.StringSet.union (free_vars l) (free_vars r)

let equal (bool_expr1 : t) (bool_expr2 : t) = bool_expr1 = bool_expr2
let compare (bool_expr1 : t) (bool_expr2 : t) = compare bool_expr1 bool_expr2
let or_level = 1
let and_level = 2
let not_level = 3

let rec pp_at level ppf bool_expr =
  let open Common.Pretty in
  match bool_expr with
  | True -> Fmt.string ppf "true"
  | False -> Fmt.string ppf "false"
  | Atom atom -> pp_atom ppf atom
  | Not (Atom _ as sub) -> Fmt.pf ppf "not (%a)" (pp_at (not_level + 1)) sub
  | Not sub -> Fmt.pf ppf "not %a" (pp_at (not_level + 1)) sub
  | And (l, r) ->
      paren_if (level > and_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a and %a@]" (pp_at and_level) l
        (pp_at (and_level + 1))
        r
  | Or (l, r) ->
      paren_if (level > or_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a or %a@]" (pp_at or_level) l (pp_at (or_level + 1)) r

let pp = pp_at 0
