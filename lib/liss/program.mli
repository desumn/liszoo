type t = { pre : Formula.t; body : Cmd.t; post : Formula.t }

val equal : t -> t -> bool
val pp : t Fmt.t
