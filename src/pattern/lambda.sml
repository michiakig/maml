structure Lambda =
struct

   datatype prim = add | sub | mul | divv

   fun applyPrim (add, x, y) = x + y
     | applyPrim (sub, x, y) = x - y
     | applyPrim (mul, x, y) = x * y
     | applyPrim (divv, x, y) = x div y

   type pat = string * string list

   structure Expr =
   struct
      (* name of type, list of ctors (name of ctor, list of args) *)
      datatype decl = Data of string * (string * string list) list

      (* expressions in the language *)
      datatype t =
               Num of int
             | Var of string
             | App of t * t list
             (* primitive app, allow for two args *)
             | PApp of prim * t * t
             | Lam of string list * t
             (* extended lambda *)
             | Let of string * t * t
             | Rec of string * t
             | Bar of t * t
             | Case of t * (pat * t) list

      fun show (Num n) = Int.toString n
        | show (Var x) = x
        | show (App (f, args)) = "(" ^ show f ^ " " ^ String.concatWith " " (map show args) ^ ")"
        | show (PApp (add, e1, e2)) = "(+ " ^ show e1 ^ " " ^ show e2 ^ ")"
        | show (PApp (sub, e1, e2)) = "(- " ^ show e1 ^ " " ^ show e2 ^ ")"
        | show (PApp (mul, e1, e2)) = "(* " ^ show e1 ^ " " ^ show e2 ^ ")"
        | show (PApp (divv, e1, e2)) = "(/ " ^ show e1 ^ " " ^ show e2 ^ ")"
        | show (Let (x, e1, e2)) = "let " ^ x ^ " = " ^ show e1 ^ " in " ^ show e2
        | show (Rec (x, e)) = "rec " ^ x ^ "." ^ show e
        | show (Bar (e1, e2)) = show e1 ^ " [] " ^ show e2
        | show (Case (e, branches)) = "case " ^ show e ^ " of" ^ (concat (map (fn ((c, args), e) => " | " ^ c ^ " " ^ String.concatWith " " args ^ " => " ^ show e) branches))
        | show (Lam (args, e)) = "λ" ^ String.concatWith " " args ^ "." ^ show e
   end

   exception Unbound
   exception AppliedNonFunction
   exception NonNumber
   exception NotImplemented
   exception IncorrectNumArgs
   exception NoMatchingBranch

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

   fun extends (env, xs, vs) =
       foldl (fn ((formal, arg), acc) => extend (acc, formal, arg)) env (ListPair.zip (xs, vs))

   structure Value =
   struct
      (*
       * values that exist at runtime or are the result of a program
       *)
      datatype t =
               Num of int
             | Data of {typ : string, ctor : string, args : t list}
             | Ctor of {typ : string, name : string, numargs : int}
             | Closure of string list * Expr.t * t Env.map

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
        | show (Ctor {typ, name, numargs}) = name

      fun eq (Num x, Num y) = x = y
        | eq (Data {typ, ctor, args}, Data {typ=typ', ctor=ctor', args=args'}) =
          typ = typ' andalso ctor = ctor' andalso ListPair.allEq eq (args, args')
        | eq _ = false
   end

local
   open Expr

   (*
    * substitute x for y in e
    *)
   fun substitute (x, y, v as Var x') = if x = x' then y else v
     | substitute (x, y, c as Num _) = c
     | substitute (x, y, App (f, args)) = App (substitute (x, y, f), map (fn a => substitute (x, y, a)) args)
     | substitute (x, y, Bar (e1, e2)) = Bar (substitute (x, y, e1), substitute (x, y, e2))
     | substitute (x, y, Case (v, ps)) = Case (v, (map (fn (p, e) => (p, substitute (x, y, e))) ps))
     | substitute (x, y, PApp (p, e1, e2)) = PApp (p, substitute (x, y, e1), substitute (x, y, e2))

     | substitute (x, y, l as Lam (args, e)) =
       if List.exists (fn x' => x = x') args
          then l
       else Lam (args, substitute (x, y, e))

     | substitute (x, y, l as Let (x', e1, e2)) =
       if x = x'
          then l
       else Let (x', substitute (x, y, e1), substitute (x, y, e2))

     | substitute (x, y, r as Rec (x', e)) =
       if x = x'
          then r
       else Rec (x', substitute (x, y, e))

   fun init ds =
       let
          fun ctor typ (name, args) = Value.Ctor {typ=typ, name=name, numargs=length args}
          fun data (Data (typ, ctors)) = map (ctor typ) ctors
       in
          foldl (fn (c as Value.Ctor {name, ...}, acc) => Env.insert (acc, name, c) | _ => Env.empty) Env.empty (List.concat (map data ds))
       end

   fun findBranch (_, []) = NONE
     | findBranch (ctor, (b as ((ctor', args), e)) :: bs) =
       if ctor = ctor'
          then SOME b
       else findBranch (ctor, bs)

   fun eval' (env, Var v) = lookup (env, v)
     | eval' (_, Num n) = Value.Num n
     | eval' (env, l as Lam (args, e)) = Value.Closure (args, e, env)
     | eval' (env, l as Let (x, e1, e2)) = eval' (extend (env, x, eval' (env, e1)), e2)

     | eval' (env, PApp (p, x, y)) =
       (case (eval' (env, x), eval' (env, y)) of
            (Value.Num x', Value.Num y') => Value.Num (applyPrim (p, x', y'))
          | _ => raise NonNumber)

     | eval' (env, App (f, args)) =
       (case eval' (env, f) of

            Value.Closure (formals, e, env') =>
            if length args <> length formals
               then raise IncorrectNumArgs
            else eval' (extends (env, formals, map (fn x => eval' (env, x)) args), e)

          | Value.Ctor {typ, name, numargs} =>
            if length args <> numargs
               then raise IncorrectNumArgs
            else Value.Data {typ=typ, ctor=name, args= map (fn x => eval' (env, x)) args}

          | _ => raise AppliedNonFunction)

     | eval' (env, r as Rec (x, e)) = eval' (env, substitute (x, r, e))

     | eval' (env, Case (e, branches)) = (case eval' (env, e) of
                                              Value.Data {typ, ctor, args} =>
                                              (case findBranch (ctor, branches) of
                                                   NONE => raise NoMatchingBranch
                                                 | SOME ((ctor, args'), e') => eval' (extends (env, args', args), e'))
                                            | _ => raise NotImplemented)

     | eval' (_, Bar _) = raise NotImplemented

in

   fun eval (ds, e) = eval' (init ds, e)

end

end
