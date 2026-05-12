type t =
  | Top | Bot
  | Atom of Atom.t
  | Not of t
  | And of t * t
  | Or of t * t
  | Imp of t * t
  | Iff of t * t

let rec free_vars formula =
  match formula with
  | Top | Bot -> Common.StringSet.empty
  | Atom atom -> Atom.free_vars atom
  | Not formula -> free_vars formula
  | And (l, r)
  | Or (l, r)
  | Imp (l, r)
  | Iff (l, r) -> Common.StringSet.union (free_vars l) (free_vars r)

let rec subst name term formula =
  match formula with
  | Top | Bot -> formula
  | Atom atom -> Atom (Atom.subst name term atom)
  | Not f -> Not (subst name term f)
  | And (l, r) -> And (subst name term l, subst name term r)
  | Or (l, r) -> Or (subst name term l, subst name term r)
  | Imp (l, r)-> Imp (subst name term l, subst name term r)
  | Iff (l, r) -> Iff (subst name term l, subst name term r)

let iff_level = 0
let imp_level = 1
let or_level = 2
let and_level = 3
let not_level = 4
let atom_level = 5
let top_level = 5
let bot_level = 5

let paren_if cond ppf do_ =
  if cond then (Fmt.pf ppf "("; do_ (); Fmt.pf ppf ")")
  else do_ ()

let rec pp_at level ppf formula =
  match formula with
  | Top -> Fmt.string ppf "true"
  | Bot -> Fmt.string ppf "false"
  | Atom atom -> Atom.pp ppf atom
  | Not ((Atom _) as sub) ->
    Fmt.pf ppf "~(%a)" (pp_at (not_level + 1)) sub
  | Not sub ->
    Fmt.pf ppf "~%a" (pp_at (not_level + 1)) sub
  | And (l, r) ->
    paren_if (level > and_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a /\\ %a@]" (pp_at and_level) l (pp_at (and_level + 1)) r
  | Or (l, r) ->
    paren_if (level > or_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a \\/ %a@]" (pp_at or_level) l (pp_at (or_level + 1)) r
  | Imp (l, r) ->
    paren_if (level > imp_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a => %a@]" (pp_at (imp_level + 1)) l (pp_at imp_level) r
  | Iff (l, r) ->
    paren_if (level > iff_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a <=> %a@]" (pp_at iff_level) l (pp_at (iff_level + 1)) r



let pp = pp_at 0

let equal (formula1 : t) (formula2 : t) = formula1 = formula2
let compare (formula1 : t) (formula2 : t) = compare formula1 formula2
