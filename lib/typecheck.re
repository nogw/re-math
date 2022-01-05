open Ast;

[@deriving show({with_path: false})]
type typ =
  | TVar
  | TInt;

module Context: {
  type t;

  let empty: t;
  let lookup: (t, string) => typ;
  let extend: (t, string, typ) => t;
} = {
  type t = list((string, typ));
  let empty = [];
  let lookup = (context, x) =>
    switch (List.find_opt(((k, _)) => k == x, context)) {
    | Some((_, x)) => x
    | None => failwith("variable " ++ x ++ " not found")
    };
  let extend = (context, x, ty) => [(x, ty), ...context];
};

open Context;

let is_value: expression => bool =
  fun
  | Int(_)
  | Var(_) => true
  | BinaryOperation(_) => false;

let rec typeof = context => {
  fun
  | Var(_) => TVar
  | Int(_) => TInt
  | BinaryOperation(b, e, e1) => typeof_binary_operation(context, b, e, e1);
}

and typeof_binary_operation = (context, b, e, e1) => {
  let (t1, t2) = (typeof(context, e), typeof(context, e1));
  switch (b, t1, t2) {
  | (Add | Subt | Mult | Div | Mod | Exp, TInt, TInt) => TInt
  | _ => failwith("operator and operand type mismatch")
  };
};

let rec subst = (expression, value, x) => {
  switch (expression) {
  | Var(y) => y == x ? value : expression
  | Int(_) => expression
  | BinaryOperation(binary_operation, e, e1) =>
    BinaryOperation(
      binary_operation,
      subst(e, value, x),
      subst(e1, value, x),
    )
  };
};

let typecheck = e => ignore(typeof(empty, e));
