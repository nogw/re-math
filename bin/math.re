open Parse;
open Typecheck;
open Ast;

let rec step: Ast.expression => Ast.expression =
  fun
  | Int(_) | Var(_) => failwith("does not step")
  | Bop(bop, e, e') when !is_value(e)                  => step(Bop(bop, step(e), e'))
  | Bop(bop, e, e') when !is_value(e')                 => step(Bop(bop, e, step(e')))
  | Bop(bop, e, e') when !is_value(e) && !is_value(e') => step(Bop(bop, step(e), step(e')))
  | Bop(bop, e, e') when is_value(e) && is_value(e')   => step_binop(bop, e, e')
  | Bop(bop, e, e') when is_value(e)                   => Bop(bop, e, step(e'))
  | Bop(bop, e, e')                                    => Bop(bop, e, e')

and step_binop = (bop, e, e1) =>
  switch (bop, e, e1) {
  | (Add,  Int(a), Int(b)) => Int(a + b)
  | (Subt, Int(a), Int(b)) => Int(a - b)
  | (Mult, Int(a), Int(b)) => Int(a * b)
  | (Div,  Int(a), Int(b)) => Int(a / b)
  | (Mod,  Int(a), Int(b)) => Int(a mod b)
  | (Exp,  Int(a), Int(b)) => Int(int_of_float(float_of_int(a) ** float_of_int(b)))
  | _ => failwith("Operator and operand type mismatch")
  };

let eval = (e: expression): expression => {
  switch (e) {
  | Int(_)
  | Var(_) => e
  | Bop(bop, e, e1) => step(Bop(bop, e, e1))
  };
};

let parse = (s: string): expression => {
  let lexbuf = Lexing.from_string(s);
  let ast = Parser.prog(Lexer.token, lexbuf);
  ast;
};

let interprete = (s: string): expression => {
  let e = parse(s);
  typecheck(e);
  eval(e);
};