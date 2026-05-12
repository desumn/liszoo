type 'a t = { node : 'a; loc : Lexing.position * Lexing.position }

val make : Lexing.position * Lexing.position -> 'a -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val dummy : 'a -> 'a t
