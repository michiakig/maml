structure Lambda =
struct

   datatype prim = add | sub | mul | divv

   fun applyPrim (add, x, y) = x + y
     | applyPrim (sub, x, y) = x - y
     | applyPrim (mul, x, y) = x * y
     | applyPrim (divv, x, y) = x div y

   type pat = unit (* undefined for now *)

   datatype expr =
            Num of int
          | Var of string
          | App of expr * expr
          (* primitive app, allow for two args *)
          | PApp of prim * expr * expr
          | Lam of string * expr
          (* extended lambda *)
          | Let of string * expr * expr
          | Letrec of string * expr * expr (* restrict to one binding in letrec *)
          | Fatbar of expr * expr
          | Case of string * pat * expr

   fun show (Num n) = Int.toString n
     | show (Var x) = x
     | show (App (e1, e2)) = "(" ^ show e1 ^ " " ^ show e2 ^ ")"
     | show (Lam (x, e)) = "λ" ^ x ^ "." ^ show e

   fun occursFree (x, Var y) = x = y
     | occursFree (x, Num _) = false
     | occursFree (x, App (e1, e2)) = occursFree (x, e1) orelse occursFree (x, e2)
     | occursFree (x, Lam (y, e)) = x <> y andalso occursFree (x, e)

   (*
    * substitute x for y in e
    *)
   fun substitute (x, y, v as Var x') = if x = x' then y else v
     | substitute (x, y, c as Num _) = c
     | substitute (x, y, App (f, a)) = App (substitute (x, y, f), substitute (x, y, a))
     | substitute (x, y, l as Lam (x', e)) =
       if x = x'
          then l
       else Lam (x', substitute (x, y, e))

   exception Unbound
   exception AppliedNonFunction
   exception NonNumber

   fun eval (Var _) = raise Unbound
     | eval (n as Num _) = n
     | eval (l as Lam _) = l
     | eval (PApp (p, x, y)) =
       (case (eval x, eval y) of
            (Num x', Num y') => Num (applyPrim (p, x', y'))
          | _ => raise NonNumber)
     | eval (App (f, a)) =
       case eval f of
           Lam (x, e) => eval (substitute (x, eval a, e))
         | _ => raise AppliedNonFunction

end
