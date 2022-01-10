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

let read_file = (filename: string): string => {
  let ic = open_in(filename);
  let try_read = () =>
    try(Some(input_line(ic))) {
    | End_of_file => None
    };
  let rec aux = acc => {
    switch (try_read()) {
    | Some(s) => aux(acc ++ s)
    | None =>
      close_in(ic);
      acc;
    };
  };
  aux("");
};

open Cmdliner

let file = Arg.(value & pos(0, some(string), None) & info([], ~doc="file: file that contains the code"));
let ast  = Arg.(value & flag & info(["a", "ast"], ~doc="ast: display ast of code"))

let execute = (file, ast) => {
  switch (file) {
    | Some(file) => ast 
      ? { read_file(file) |> parse |> show_expression |> print_endline } 
      : { read_file(file) |> interprete |> show_expression |> print_endline }
    | None => exit(1)
  }
}

let cmd = () => {
  Term.(const(execute) $ file $ ast)
}

let () = { 
  let doc = "";
  let man = [
    `S (Manpage.s_synopsis),
    `P ("parse-math [FILE.MATH] [ARG]"),
    `S (Manpage.s_bugs), `P ("Report bugs to <github.com/nogw/reason-menhir/issues>.")]
    
  Term.exit @@ Term.eval((cmd(), Term.info("parse-math", ~version="v1.0.4", ~doc, ~man)));
}