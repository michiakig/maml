structure LambdaTests =
struct

open Lambda
open QCheck infix ==>

fun main _ = ( check (List.getItem,
                      SOME (fn (x, y) => "(" ^ show x ^ "," ^ show y ^ ")"))
                     ("eval", pred (fn (x, y) => y = eval x))
[
  (App (Lam ("x", Var "x"), Cst (Num 1)), Cst (Num 1))
 ,(App (Lam ("x", Lam ("y", Var "x")), Cst (Num 1)), Lam ("y", Cst (Num 1)))
 ,(App (Lam ("x", Lam ("y", Var "y")), Cst (Num 1)), Lam ("y", Var "y"))
 ,(App (Lam ("x", App (Cst Succ, Var "x")), Cst (Num 0)), Cst (Num 1))
]
; OS.Process.success )

end
