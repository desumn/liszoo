type t =
  | Top
  | Bot
  | Atom of atom
  | Not of t
  | And of t * t
  | Or of t * t
  | Imp of t * t
  | Iff of t * t

and term =
  | Var of string
  | Const of int
  | Neg of term
  | Add of term * term
  | Sub of term * term
  | Mul of term * term

and atom =
  | Eq of term * term
  | Neq of term * term
  | Lt of term * term
  | Le of term * term
  | Gt of term * term
  | Ge of term * term

module Term = struct
  type t = term =
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

  let rec subst var ~by ~on =
    match on with
    | Var target when var = target -> by
    | Var target -> Var target
    | Const c -> Const c
    | Neg t -> Neg (subst var ~on:t ~by)
    | Add (l, r) -> Add (subst var ~on:l ~by, subst var ~on:r ~by)
    | Sub (l, r) -> Sub (subst var ~on:l ~by, subst var ~on:r ~by)
    | Mul (l, r) -> Mul (subst var ~on:l ~by, subst var ~on:r ~by)

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
        Fmt.pf ppf "@[<2>%a +@ %a@]" (pp_at add_level) l
          (pp_at (add_level + 1))
          r
    | Sub (l, r) ->
        paren_if (level > sub_level) ppf @@ fun () ->
        Fmt.pf ppf "@[<2>%a -@ %a@]" (pp_at sub_level) l
          (pp_at (sub_level + 1))
          r
    | Mul (l, r) ->
        paren_if (level > mul_level) ppf @@ fun () ->
        Fmt.pf ppf "@[<2>%a *@ %a@]" (pp_at mul_level) l
          (pp_at (mul_level + 1))
          r

  let pp = pp_at 0
  let equal (term1 : t) (term2 : t) = term1 = term2
  let compare term1 term2 = compare term1 term2
end

module Atom = struct
  type t = atom =
    | Eq of Term.t * Term.t
    | Neq of Term.t * Term.t
    | Lt of Term.t * Term.t
    | Le of Term.t * Term.t
    | Gt of Term.t * Term.t
    | Ge of Term.t * Term.t

  let free_vars atom =
    match atom with
    | Eq (l, r) | Neq (l, r) | Lt (l, r) | Le (l, r) | Gt (l, r) | Ge (l, r) ->
        Common.StringSet.union (Term.free_vars l) (Term.free_vars r)

  let subst name subst_term atom =
    match atom with
    | Eq (l, r) ->
        Eq
          ( Term.subst name ~by:subst_term ~on:l,
            Term.subst name ~by:subst_term ~on:r )
    | Neq (l, r) ->
        Neq
          ( Term.subst name ~by:subst_term ~on:l,
            Term.subst name ~by:subst_term ~on:r )
    | Lt (l, r) ->
        Lt
          ( Term.subst name ~by:subst_term ~on:l,
            Term.subst name ~by:subst_term ~on:r )
    | Le (l, r) ->
        Le
          ( Term.subst name ~by:subst_term ~on:l,
            Term.subst name ~by:subst_term ~on:r )
    | Gt (l, r) ->
        Gt
          ( Term.subst name ~by:subst_term ~on:l,
            Term.subst name ~by:subst_term ~on:r )
    | Ge (l, r) ->
        Ge
          ( Term.subst name ~by:subst_term ~on:l,
            Term.subst name ~by:subst_term ~on:r )

  let pp ppf atom =
    match atom with
    | Eq (l, r) -> Fmt.pf ppf "%a = %a" Term.pp l Term.pp r
    | Neq (l, r) -> Fmt.pf ppf "%a <> %a" Term.pp l Term.pp r
    | Lt (l, r) -> Fmt.pf ppf "%a < %a" Term.pp l Term.pp r
    | Le (l, r) -> Fmt.pf ppf "%a <= %a" Term.pp l Term.pp r
    | Gt (l, r) -> Fmt.pf ppf "%a > %a" Term.pp l Term.pp r
    | Ge (l, r) -> Fmt.pf ppf "%a >= %a" Term.pp l Term.pp r

  let equal (atom1 : t) (atom2 : t) = atom1 = atom2
  let compare atom1 atom2 = compare atom1 atom2
end

let rec free_vars formula =
  match formula with
  | Top | Bot -> Common.StringSet.empty
  | Atom atom -> Atom.free_vars atom
  | Not formula -> free_vars formula
  | And (l, r) | Or (l, r) | Imp (l, r) | Iff (l, r) ->
      Common.StringSet.union (free_vars l) (free_vars r)

let rec subst name term formula =
  match formula with
  | Top | Bot -> formula
  | Atom atom -> Atom (Atom.subst name term atom)
  | Not f -> Not (subst name term f)
  | And (l, r) -> And (subst name term l, subst name term r)
  | Or (l, r) -> Or (subst name term l, subst name term r)
  | Imp (l, r) -> Imp (subst name term l, subst name term r)
  | Iff (l, r) -> Iff (subst name term l, subst name term r)

let iff_level = 0
let imp_level = 1
let or_level = 2
let and_level = 3
let not_level = 4

let rec pp_at level ppf formula =
  let open Common.Pretty in
  match formula with
  | Top -> Fmt.string ppf "true"
  | Bot -> Fmt.string ppf "false"
  | Atom atom -> Atom.pp ppf atom
  | Not (Atom _ as sub) -> Fmt.pf ppf "~(%a)" (pp_at (not_level + 1)) sub
  | Not sub -> Fmt.pf ppf "~%a" (pp_at (not_level + 1)) sub
  | And (l, r) ->
      paren_if (level > and_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a /\\ %a@]" (pp_at and_level) l
        (pp_at (and_level + 1))
        r
  | Or (l, r) ->
      paren_if (level > or_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a \\/ %a@]" (pp_at or_level) l (pp_at (or_level + 1)) r
  | Imp (l, r) ->
      paren_if (level > imp_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a => %a@]" (pp_at (imp_level + 1)) l (pp_at imp_level) r
  | Iff (l, r) ->
      paren_if (level > iff_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a <=> %a@]" (pp_at iff_level) l
        (pp_at (iff_level + 1))
        r

let pp = pp_at 0
let equal (formula1 : t) (formula2 : t) = formula1 = formula2
let compare (formula1 : t) (formula2 : t) = compare formula1 formula2
