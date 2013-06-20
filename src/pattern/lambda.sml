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
          | Bar of expr * expr
          | Case of string * (pat * expr) list

   fun show (Num n) = Int.toString n
     | show (Var x) = x
     | show (App (e1, e2)) = "(" ^ show e1 ^ " " ^ show e2 ^ ")"
     | show (PApp (add, e1, e2)) = "(+ " ^ show e1 ^ " " ^ show e2 ^ ")"
     | show (PApp (sub, e1, e2)) = "(- " ^ show e1 ^ " " ^ show e2 ^ ")"
     | show (PApp (mul, e1, e2)) = "(* " ^ show e1 ^ " " ^ show e2 ^ ")"
     | show (PApp (divv, e1, e2)) = "(/ " ^ show e1 ^ " " ^ show e2 ^ ")"
     | show (Let (x, e1, e2)) = "let " ^ x ^ " = " ^ show e1 ^ " in " ^ show e2
     | show (Letrec (x, e1, e2)) = "letrec " ^ x ^ " = " ^ show e1 ^ " in " ^ show e2
     | show (Bar (e1, e2)) = show e1 ^ " [] " ^ show e2
     | show (Case (v, ps)) = "case " ^ v ^ " of" ^ (concat (map (fn (p, e) => " | _ => " ^ show e) ps))
     | show (Lam (x, e)) = "λ" ^ x ^ "." ^ show e

   (*
    * substitute x for y in e
    *)
   fun substitute (x, y, v as Var x') = if x = x' then y else v
     | substitute (x, y, c as Num _) = c
     | substitute (x, y, App (f, a)) = App (substitute (x, y, f), substitute (x, y, a))
     | substitute (x, y, Bar (e1, e2)) = Bar (substitute (x, y, e1), substitute (x, y, e2))
     | substitute (x, y, Case (v, ps)) = Case (v, (map (fn (p, e) => (p, substitute (x, y, e))) ps))
     | substitute (x, y, PApp (p, e1, e2)) = PApp (p, substitute (x, y, e1), substitute (x, y, e2))

     | substitute (x, y, l as Lam (x', e)) =
       if x = x'
          then l
       else Lam (x', substitute (x, y, e))

     | substitute (x, y, l as Let (x', e1, e2)) =
       if x = x'
          then l
       else Let (x', substitute (x, y, e1), substitute (x, y, e2))

     | substitute (x, y, l as Letrec (x', e1, e2)) =
       if x = x'
          then l
       else Letrec (x', substitute (x, y, e1), substitute (x, y, e2))

   exception Unbound
   exception AppliedNonFunction
   exception NonNumber
   exception NotImplemented

   fun eval (Var _) = raise Unbound
     | eval (n as Num _) = n
     | eval (l as Lam _) = l
     | eval (l as Let (x, e1, e2)) = eval (substitute (x, eval e1, e2))

     | eval (Letrec _) = raise NotImplemented

     | eval (Bar _) = raise NotImplemented

     | eval (Case _) = raise NotImplemented

     | eval (PApp (p, x, y)) =
       (case (eval x, eval y) of
            (Num x', Num y') => Num (applyPrim (p, x', y'))
          | _ => raise NonNumber)

     | eval (App (f, a)) =
       case eval f of
           Lam (x, e) => eval (substitute (x, eval a, e))
         | _ => raise AppliedNonFunction

end
