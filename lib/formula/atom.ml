type t =
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
let compare (atom1 : t) (atom2 : t) = compare atom1 atom2
