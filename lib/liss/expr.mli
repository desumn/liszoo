type t =
  | Var of string
  | Const of int
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t

val free_vars : t -> Common.StringSet.t
val equal : t -> t -> bool
val compare : t -> t -> int
val pp : t Fmt.t
