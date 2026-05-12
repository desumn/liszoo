type 'a t = { node : 'a; loc : Lexing.position * Lexing.position }

let make loc node = { node; loc }
let map f located = { located with node = f located.node }
let dummy node = { node; loc = (Lexing.dummy_pos, Lexing.dummy_pos) }
