let rec to_term (expr : Expr.t) =
  match expr with
  | Var var -> Formula.Term.Var var
  | Const const -> Formula.Term.Const const
  | Neg sub -> Formula.Term.Neg (to_term sub)
  | Add (l, r) -> Formula.Term.Add (to_term l, to_term r)
  | Sub (l, r) -> Formula.Term.Sub (to_term l, to_term r)
  | Mul (l, r) -> Formula.Term.Mul (to_term l, to_term r)

let rec to_atom (atom : Bool_expr.atom) =
  match atom with
  | Eq (l, r) -> Formula.Atom.Eq (to_term l, to_term r)
  | Neq (l, r) -> Formula.Atom.Neq (to_term l, to_term r)
  | Lt (l, r) -> Formula.Atom.Lt (to_term l, to_term r)
  | Le (l, r) -> Formula.Atom.Le (to_term l, to_term r)
  | Gt (l, r) -> Formula.Atom.Gt (to_term l, to_term r)
  | Ge (l, r) -> Formula.Atom.Ge (to_term l, to_term r)

let rec to_formula (formula : Bool_expr.t) =
  match formula with
  | Atom atom -> Formula.Atom (to_atom atom)
  | True -> Formula.Top
  | False -> Formula.Bot
  | Not sub -> Formula.Not (to_formula sub)
  | And (l, r) -> Formula.And (to_formula l, to_formula r)
  | Or (l, r) -> Formula.Or (to_formula l, to_formula r)
