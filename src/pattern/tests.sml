structure LambdaTests =
struct

open Lambda
open Expr
open QCheck infix ==>

fun main _ = ( check (List.getItem,
                      SOME (fn (x, y) => "(" ^ show x ^ "," ^ Value.show y ^ ")"))
                     ("eval", pred (fn (x, y) => Value.eq (eval (Env.empty, x), y)))
[
  (App (Lam ("x", Var "x"), Num 1), Value.Num 1)
 (* ,(App (Lam ("x", Lam ("y", Var "x")), Num 1), Lam ("y", Num 1)) *)
 (* ,(App (Lam ("x", Lam ("y", Var "y")), Num 1), Lam ("y", Var "y")) *)

 ,(Let ("x", Num 1, PApp (add, Var "x", Var "x")), Value.Num 2)
 ,(App (Let ("x", Num 1, Lam ("y", PApp (add, Var "x", Var "y"))), Num 2), Value.Num 3)
]
; OS.Process.success )

end
