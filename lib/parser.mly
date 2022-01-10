%{
  open Ast
%}

%token <int> INT
%token PLUS MINUS ASTERISK SLASH PERCENT CAROT
%token LPAREN RPAREN
%token EOF

%start <Ast.expression> prog %%

prog: 
    | e = expr ; EOF { e } 
    ;

expr:
    | i = INT { Int i }
    | b = binary_operation ; e = expr ; e1 = expr { Bop ( b, e, e1 ) }
    | LPAREN ; e = expr ; RPAREN { e }
    ;

%inline binary_operation:
    | PLUS     { Add }
    | MINUS    { Subt }
    | ASTERISK { Mult }
    | SLASH    { Div }
    | PERCENT  { Mod }
    | CAROT    { Exp }