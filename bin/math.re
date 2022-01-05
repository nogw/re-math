open Parse;
open Ast;

let rec step: Ast.expression => Ast.expression =
  fun
  | Int(_)
  | Var(_) => failwith("does not step")
  | BinaryOperation(binary_operation, e, e1)
      when Typecheck.is_value(e) && Typecheck.is_value(e1) =>
    step_binary_operation(binary_operation, e, e1)
  | BinaryOperation(binary_operation, e, e1) when Typecheck.is_value(e) =>
    BinaryOperation(binary_operation, e, step(e1))
  | BinaryOperation(binary_operation, e, e1) =>
    BinaryOperation(binary_operation, e, e1)

and step_binary_operation = (binary_operation, e, e1) =>
  switch (binary_operation, e, e1) {
  | (Add, Int(a), Int(b)) => Int(a + b)
  | (Subt, Int(a), Int(b)) => Int(a - b)
  | (Mult, Int(a), Int(b)) => Int(a * b)
  | (Div, Int(a), Int(b)) => Int(a / b)
  | (Mod, Int(a), Int(b)) => Int(a mod b)
  | (Exp, Int(a), Int(b)) =>
    Int(int_of_float(float_of_int(a) ** float_of_int(b)))
  | _ => failwith("Operator and operand type mismatch")
  };

let eval = (e: expression): expression => {
  switch (e) {
  | Int(_)
  | Var(_) => e
  | BinaryOperation(binary_operation, e, e1) =>
    step_binary_operation(binary_operation, e, e1)
  };
};

let parse = (s: string): expression => {
  let lexbuf = Lexing.from_string(s);
  let ast = Parser.prog(Lexer.token, lexbuf);
  ast;
};

let interprete = (s: string): expression => {
  let e = parse(s);
  Typecheck.typecheck(e);
  eval(e);
};

let input = prefix => {
  print_string(prefix);
  let str = read_line();
  str;
};

let cli_options =
  [
    ["", "--help", "show usage information and exits"],
    ["", "--ast", "show the AST for the file and exits"],
  ]
  |> List.map(String.concat("\t"))
  |> String.concat("\n");

let cli_usage = () => {
  [
    "USAGE: math file.math [options]",
    "",
    "OPTIONS:",
    cli_options,
    "",
    "EXAMPLE:",
    "\t$ math program.math",
  ]
  |> String.concat("\n")
  |> print_endline;
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

let () = {
  let len = Array.length(Sys.argv);

  if (len <= 1) {
    cli_usage();
  } else if (len <= 2) {
    read_file(Sys.argv[1]) |> interprete |> show_expression |> print_endline;
  } else {
    let (filename, opt) = (Sys.argv[1], Sys.argv[2]);

    switch (opt) {
    | "--ast" =>
      read_file(filename) |> parse |> show_expression |> print_endline
    | "--help" => cli_usage()
    | _ => print_endline("Invalid option, try --help")
    };
  };
};
