```Ocaml
ast  - (BinaryOperation (Add, (Int 2), (Int 3)))
expect   - Int(5)
received - Int(5)

ast  - (BinaryOperation (Add, (BinaryOperation (Add, (Int 1), (Int 2))), (Int 3)))
expect   - Int(6)
received - Failure("Operator and operand type mismatch") 
```
