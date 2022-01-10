[@deriving show({with_path: false})]
type variable = string;

[@deriving show({with_path: false})]
type binary_operations =
  | Add
  | Subt
  | Mult
  | Div
  | Mod
  | Exp;

[@deriving show({with_path: false})]
type expression =
  | Int(int)
  | Var(variable)
  | Bop(binary_operations, expression, expression);
