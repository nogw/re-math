{ 
  open Parser
  open Lexing

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1 }
} 

let white   = [' ' '\t']+
let digit   = ['0' - '9']
let int     = '-' ? digit+
let letter  = ['a' - 'z' 'A' - 'Z']
let id      = letter+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | "("     { LPAREN }
  | ")"     { RPAREN }
  | "-"     { MINUS }
  | "+"     { PLUS }
  | "*"     { ASTERISK }
  | "/"     { SLASH }
  | "%"     { PERCENT }
  | "^"     { CAROT }
  | white   { token lexbuf }
  (* | "//"    { s_line_comment lexbuf } *)
  | "/*"    { m_line_comment lexbuf } 
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | newline { next_line lexbuf; token lexbuf }
  | eof     { EOF }
  | _       { raise (SyntaxError ("[LEXER ERROR] Illegal character: " ^ Lexing.lexeme lexbuf)) }

(* TODO *)
(* and s_line_comment = parse
  | newline { token lexbuf }
  | eof     { raise (SyntaxError ("TODO")) } 
  | _       { s_line_comment lexbuf } *) *)

and m_line_comment = parse
  | "*/"    { token lexbuf } 
  | newline { next_line lexbuf; m_line_comment lexbuf } 
  | eof     { raise ( SyntaxError ("[LEXER ERROR] Unclosed comment at " ^ Lexing.lexeme lexbuf)) }
  | _       { m_line_comment lexbuf } 