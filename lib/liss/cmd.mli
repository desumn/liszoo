type node =
  | Skip
  | Assign of string * Expr.t
  | Seq    of t * t
  | If     of Bool_expr.t * t * t
  | While  of Bool_expr.t * Formula.t * t
  | Assert of Formula.t
  | Assume of Formula.t
and t = node Common.Located.t

val equal : t -> t -> bool

val pp : t Fmt.t
