structure LambdaTests =
struct

open Lambda
open Expr
open QCheck infix ==>

fun main _ = ( check (List.getItem,
                      SOME (fn (e, v) => "(" ^ show e ^ "," ^ Value.show v ^ ")"))
                     ("eval", pred (fn (e, v) => Value.eq (eval ([], e), v)))
[
  (App (Lam (["x"], Var "x"), [Num 1]), Value.Num 1)
 (* ,(App (Lam ("x", Lam ("y", Var "x")), Num 1), Lam ("y", Num 1)) *)
 (* ,(App (Lam ("x", Lam ("y", Var "y")), Num 1), Lam ("y", Var "y")) *)

 ,(Let ("x", Num 1, PApp (add, Var "x", Var "x")), Value.Num 2)
 ,(App (Lam (["x", "y"], PApp (add, Var "x", Var "y")), [Num 1, Num 2]), Value.Num 3)
 ,(App (Let ("x", Num 1, Lam (["y"], PApp (add, Var "x", Var "y"))), [Num 2]), Value.Num 3)
]

; check (List.getItem, SOME (fn (ds, e, v) => "(" ^ show e ^ "," ^ Value.show v ^ ")"))
        ("eval with decls", pred (fn (ds, e, v) => Value.eq (eval (ds, e), v)))
[
  ([Data ("list",[("Cons",["x","y"]),("Nil",[])])],
   App (Var "Cons", [Num 1, App (Var "Nil", [])]),
   Value.Data {typ="list", ctor="Cons", args=[Value.Num 1, Value.Data {typ="list", ctor="Nil", args=[]}]})
]

; OS.Process.success )

end
