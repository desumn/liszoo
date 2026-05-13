{
open Parser
exception Lexing_error of string * Lexing.position

let transform_number num_lexeme =
  let res = Buffer.create (String.length num_lexeme) in
  String.iter (fun c -> if c <> '_' then Buffer.add_char res c) num_lexeme;
  int_of_string (Buffer.contents res)

}

let digit   = ['0'-'9']
let letter  = ['a'-'z' 'A'-'Z']
let ident   = letter (letter | digit | '_')*
let number = digit (digit | '_')*

rule token = parse
  | [' ' '\t']+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "(*" { comment 1 lexbuf }
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {STAR}
  | ":=" {ASSIGN}
  | "=" {EQ}
  | "/=" {NEQ_PROG}
  | "<" {LT}
  | "<=" {LE}
  | ">" {GT}
  | ">=" {GE}
  | "<>" {NEQ_LOG}
  | "~" {TILDE}
  | "/\\" {WEDGE}
  | "\\/" {VEE}
  | "=>" {IMP}
  | "<=>" {IFF}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "{" {LBRACE}
  | "}" {RBRACE}
  | ";" {SEMI}
  | "skip" {SKIP}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "end" {END}
  | "while" {WHILE}
  | "invariant" {INVARIANT}
  | "do" {DO}
  | "assert" {ASSERT}
  | "assume" {ASSUME}
  | "pre" {PRE}
  | "post" {POST}
  | "not" {NOT}
  | "and" {AND}
  | "or" {OR}
  | "true" {TRUE}
  | "false" {FALSE}
  | number as num {INT (transform_number num)}
  | ident as ident {IDENT ident}
  | eof {EOF}
  | _ as c {raise (Lexing_error (String.make 1 c, Lexing.lexeme_start_p lexbuf))}
and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf}
  | "*)" { if depth = 1 then token lexbuf else comment (depth - 1) lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment depth lexbuf }
  | eof { raise (Lexing_error ("eof in comment", Lexing.lexeme_start_p lexbuf)) }
  | _ { comment depth lexbuf }
