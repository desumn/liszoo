module Expr = Expr
module Bool_expr = Bool_expr
module Cmd = Cmd
module Program = Program
module Conv = Conv

exception Parse_error of string * (Lexing.position * Lexing.position)

module I = Parser.MenhirInterpreter

let succeed (ast : Program.t) = ast

let fail (lexbuf : Lexing.lexbuf) (checkpoint : Program.t I.checkpoint) =
  match checkpoint with
  | I.HandlingError env ->
      let state_num = I.current_state_number env in
      let msg =
        match Parser_messages.message state_num with
        | msg -> msg
        | exception Not_found -> "syntaxe error (no message found)"
      in
      let pos = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      raise (Parse_error (msg, pos))
  | _ -> assert false

let supplier (lexbuf : Lexing.lexbuf) () =
  let token = Lexer.token lexbuf in
  (token, lexbuf.lex_start_p, lexbuf.lex_curr_p)

let parse_lexbuf (lexbuf : Lexing.lexbuf) =
  let checkpoint = Parser.Incremental.program lexbuf.lex_curr_p in
  I.loop_handle succeed (fail lexbuf) (supplier lexbuf) checkpoint

let parse_string string =
  let lexbuf = Lexing.from_string string in
  parse_lexbuf lexbuf
