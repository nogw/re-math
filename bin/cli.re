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
      ? { read_file(file) |> Math.parse |> Parse.Ast.show_expression |> print_endline } 
      : { read_file(file) |> Math.interprete |> Parse.Ast.show_expression |> print_endline }
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