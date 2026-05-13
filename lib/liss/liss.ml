module Expr = Expr
module Bool_expr = Bool_expr
module Cmd = Cmd
module Program = Program
module Conv = Conv

let parse_lexbuf lb = Parser.program Lexer.token lb
let parse_string s = parse_lexbuf (Lexing.from_string s)
