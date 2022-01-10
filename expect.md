```Ocaml
math     - (+ 2 3)
ast      - (Bop (Add, (Int (2)), (Int (3))))
expect   - Int(5)
received - Int(5)

math     - (+ (+ 4 2) 4)
ast      - (Bop (Add, (Bop (Add, (Int (4)), (Int (2)))), (Int (4))))
expect   - Int(10)
received - Int(10)

math     - (+ 4 (+ 4 2))
ast      - (Bop (Add, (Int (4)), (Bop (Add, (Int (4)), (Int (2))))))
expect   - Int(10)
received - Int(10)

math     - (- (+ (^ 5 6) 16) (+ (^ 5 6) (/ 98 7)))
math     - (Bop (Subt, (Bop (Add, (Bop (Exp, (Int 5), (Int 6))), (Int 16))), (Bop (Add, (Bop (Exp, (Int 5), (Int 6))), (Bop (Div, (Int 98), (Int 7)))))))
expect   - Int(2)
received - Int(2) (* ugly *)
```
