structure LambdaTests =
struct

open Lambda
open QCheck infix ==>

fun main _ = ( check (List.getItem,
                      SOME (fn (x, y) => "(" ^ show x ^ "," ^ show y ^ ")"))
                     ("eval", pred (fn (x, y) => y = eval x))
[
  (App (Lam ("x", Var "x"), Num 1), Num 1)
 ,(App (Lam ("x", Lam ("y", Var "x")), Num 1), Lam ("y", Num 1))
 ,(App (Lam ("x", Lam ("y", Var "y")), Num 1), Lam ("y", Var "y"))

 ,(Let ("x", Num 1, PApp (add, Var "x", Var "x")), Num 2)
 ,(App (Let ("x", Num 1, Lam ("y", PApp (add, Var "x", Var "y"))), Num 2), Num 3)
]
; OS.Process.success )

end
