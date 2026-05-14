type t =
  | Top
  | Bot
  | Atom of atom
  | Not of t
  | And of t * t
  | Or of t * t
  | Imp of t * t
  | Iff of t * t

and term = private
  | Var of string
  | Const of int
  | Neg of term
  | Add of term * term
  | Sub of term * term
  | Mul of term * term

and atom = private
  | Eq of term * term
  | Neq of term * term
  | Lt of term * term
  | Le of term * term
  | Gt of term * term
  | Ge of term * term

module Term : sig
  type t = term =
    | Var of string
    | Const of int
    | Neg of t
    | Add of t * t
    | Sub of t * t
    | Mul of t * t

  val free_vars : t -> Common.StringSet.t
  val subst : string -> by:t -> on:t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t Fmt.t
end

module Atom : sig
  type t = atom =
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
end

val free_vars : t -> Common.StringSet.t
val subst : string -> Term.t -> t -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val pp : t Fmt.t
