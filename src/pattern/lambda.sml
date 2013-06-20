structure Lambda =
struct

   datatype prim = add | sub | mul | divv

   fun applyPrim (add, x, y) = x + y
     | applyPrim (sub, x, y) = x - y
     | applyPrim (mul, x, y) = x * y
     | applyPrim (divv, x, y) = x div y

   type pat = unit (* undefined for now *)

   structure Expr =
   struct
      (* expressions in the language *)
      datatype t =
               Num of int
             | Var of string
             | App of t * t
             (* primitive app, allow for two args *)
             | PApp of prim * t * t
             | Lam of string * t
             (* extended lambda *)
             | Let of string * t * t
             | Letrec of string * t * t (* restrict to one binding in letrec *)
             | Bar of t * t
             | Case of string * (pat * t) list

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
   end

   exception Unbound
   exception AppliedNonFunction
   exception NonNumber
   exception NotImplemented

   structure Env = BinaryMapFn(
      struct
         type ord_key = string
         val compare = String.compare
      end)

   fun lookup (env, x) =
       case Env.find (env, x) of
           NONE => raise Unbound
         | SOME v => v

   val extend = Env.insert

   structure Value =
   struct
      (*
       * values that exist at runtime or are the result of a program
       *)
      datatype t =
               Num of int
             | Data of {typ : string, ctor : string, args : t list}
             | Closure of string * Expr.t * t Env.map

      fun show (Num n) = Int.toString n
        | show (Data {typ, ctor, args}) =
          let
             fun show' (d as Data {args, ...}) =
                 if length args > 0
                    then "(" ^ show d ^ ")"
                 else show d
               | show' v = show v
          in
             if length args = 0
                then ctor
             else
                ctor ^ " " ^ String.concatWith " " (map show' args)
          end
        | show (Closure _) = "#<fn>"

      fun eq (Num x, Num y) = x = y
        | eq (Data {typ, ctor, args}, Data {typ=typ', ctor=ctor', args=args'}) =
          typ = typ' andalso ctor = ctor' andalso ListPair.allEq eq (args, args')
        | eq _ = false
   end

local
   open Expr
in
   fun eval (env, Var v) = lookup (env, v)
     | eval (_, Num n) = Value.Num n
     | eval (env, l as Lam (x, e)) = Value.Closure (x, e, env)
     | eval (env, l as Let (x, e1, e2)) = eval (extend (env, x, eval (env, e1)), e2)

     | eval (env, PApp (p, x, y)) =
       (case (eval (env, x), eval (env, y)) of
            (Value.Num x', Value.Num y') => Value.Num (applyPrim (p, x', y'))
          | _ => raise NonNumber)

     | eval (env, App (f, a)) =
       (case eval (env, f) of
            Value.Closure (x, e, env') => eval (extend (env', x, eval (env, a)), e)
          | _ => raise AppliedNonFunction)

     | eval (env, Letrec _) = raise NotImplemented
     | eval (_, Bar _) = raise NotImplemented
     | eval (_, Case _) = raise NotImplemented

end

end
