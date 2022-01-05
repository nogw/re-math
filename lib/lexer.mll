{ 
  open Parser
} 

let white  = [' ' '\t']+
let digit  = ['0' - '9']
let int    = '-' ? digit+
let letter = ['a' - 'z' 'A' - 'Z']
let id     = letter+

rule token = parse
    | white { token lexbuf }
    | int   { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "+"   { PLUS }
    | "-"   { MINUS }
    | "*"   { ASTERISK }
    | "/"   { SLASH }
    | "%"   { PERCENT }
    | "^"   { CAROT }
    | "("   { LPAREN }
    | ")"   { RPAREN }
    | eof   { EOF }