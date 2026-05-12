type t =
  | Top | Bot
  | Atom of Atom.t
  | Not of t
  | And of t * t
  | Or of t * t
  | Imp of t * t
  | Iff of t * t

val free_vars : t -> Common.StringSet.t

val subst : string -> Term.t -> t -> t

val equal : t -> t -> bool
val compare : t -> t -> int

val pp : t Fmt.t

