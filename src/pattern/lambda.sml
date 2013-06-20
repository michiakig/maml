structure Lambda =
struct

   datatype const =
            Num of int
          | Succ
          | Pred

   datatype expr =
            Cst of const
          | Var of string
          | App of expr * expr
          | Lam of string * expr

   fun show (Cst (Num n)) = Int.toString n
     | show (Cst Succ) = "succ"
     | show (Cst Pred) = "pred"
     | show (Var x) = x
     | show (App (e1, e2)) = "(" ^ show e1 ^ " " ^ show e2 ^ ")"
     | show (Lam (x, e)) = "λ" ^ x ^ "." ^ show e

   fun occursFree (x, Var y) = x = y
     | occursFree (x, Cst _) = false
     | occursFree (x, App (e1, e2)) = occursFree (x, e1) orelse occursFree (x, e2)
     | occursFree (x, Lam (y, e)) = x <> y andalso occursFree (x, e)

   (*
    * substitute x for y in e
    *)
   fun substitute (x, y, v as Var x') = if x = x' then y else v
     | substitute (x, y, c as Cst _) = c
     | substitute (x, y, App (f, a)) = App (substitute (x, y, f), substitute (x, y, a))
     | substitute (x, y, l as Lam (x', e)) =
       if x = x'
          then l
       else Lam (x', substitute (x, y, e))

   exception Unbound
   exception AppliedNonFunction
   exception TriedToTakePredOfNonNum
   exception TriedToTakeSuccOfNonNum

   fun eval (Var _) = raise Unbound
     | eval (c as Cst _) = c
     | eval (l as Lam _) = l
     | eval (App (f, a)) =
       case eval f of
           Lam (x, e) => eval (substitute (x, eval a, e))
         | Cst Pred => (case eval a of
                            c as Cst (Num 0) => c
                          | Cst (Num n) => Cst (Num (n - 1))
                          | _ => raise TriedToTakePredOfNonNum)
         | Cst Succ => (case eval a of
                            Cst (Num n) => Cst (Num (n + 1))
                          | _ => raise TriedToTakeSuccOfNonNum)
         | _ => raise AppliedNonFunction

end
