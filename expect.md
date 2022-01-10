```Ocaml
ast      - (Bop (Add, (Int (2)), (Int (3))))
expect   - Int(5)
received - Int(5)

ast      - (Bop (Add, (Bop (Add, (Int (4)), (Int (2)))), (Int (4))))
expect   - Int(10)
received - Int(10) (* but ugly *)

ast      - (Bop (Add, (Int (4)), (Bop (Add, (Int (4)), (Int (2))))))
expect   - Int(10)
received - Int(10)
```
