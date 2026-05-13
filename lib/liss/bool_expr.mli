
type atom =
  | Eq  of Expr.t * Expr.t
  | Neq of Expr.t * Expr.t
  | Lt  of Expr.t * Expr.t
  | Le  of Expr.t * Expr.t
  | Gt  of Expr.t * Expr.t
  | Ge  of Expr.t * Expr.t

val pp_atom : atom Fmt.t

type t =
  | True
  | False
  | Atom of atom
  | Not of t
  | And of t * t
  | Or of t * t

val free_vars : t -> Common.StringSet.t

val equal : t -> t -> bool
val compare : t -> t -> int

val pp : t Fmt.t
