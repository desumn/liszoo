type t =
  | Eq of Term.t * Term.t
  | Neq of Term.t * Term.t
  | Lt of Term.t * Term.t
  | Le of Term.t * Term.t
  | Gt of Term.t * Term.t
  | Ge of Term.t * Term.t

val free_vars : t -> Common.StringSet.t

val subst : string -> Term.t -> t -> t

val pp : t Fmt.t

val equal : t -> t -> bool

val compare : t -> t -> int
