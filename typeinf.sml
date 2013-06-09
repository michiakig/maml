structure TypeInf =
struct

structure T = Type

datatype ast = Num of int * int
             | Bool of int * bool
             | Succ of int * ast
             | Pred of int * ast
             | IsZero of int * ast
             | If of int * ast * ast * ast
             | App of int * ast * ast
             (* functions need two ids, one for the bound var,
              * one for the fun expr itself *)
             | Fun of int * int * string * ast
             | Id of int * string

fun findByid (n as Num (id, _), id') = if id = id' then SOME n else NONE
  | findByid (n as Bool (id, _), id') = if id = id' then SOME n else NONE
  | findByid (n as Succ (id, _), id') = if id = id' then SOME n else NONE
  | findByid (n as Pred (id, _), id') = if id = id' then SOME n else NONE
  | findByid (n as IsZero (id, _), id') = if id = id' then SOME n else NONE
  | findByid (n as If (id, e1, e2, e3), id') =
    if id = id'
       then SOME n
    else (case findByid (e1, id') of
              SOME n => SOME n
            | NONE => case findByid (e2, id') of
                          SOME n => SOME n
                        | NONE => case findByid (e3, id') of
                                      SOME n => SOME n
                                    | NONE => NONE)
  | findByid (n as App (id, e1, e2), id') =
    if id = id'
       then SOME n
    else (case findByid (e1, id') of
              SOME n => SOME n
            | NONE => case findByid (e2, id') of
                          SOME n => SOME n
                        | NONE => NONE)
  | findByid (n as Fun (b, f, v, e), id') =
    if id' = b
       then SOME (Id (b, v))
    else if id' = f
            then SOME n
         else (case findByid (e, id') of
                   SOME n => SOME n
                 | NONE => NONE)
  | findByid (n as Id (id, _), id') = if id = id' then SOME n else NONE

fun getId (Num (id, _))       = id
  | getId (Bool (id, _))      = id
  | getId (Succ (id, _))      = id
  | getId (Pred (id, _))      = id
  | getId (IsZero (id, _))    = id
  | getId (If (id, _, _, _))  = id
  | getId (App (id, _, _))    = id
  | getId (Fun (_, id, _, _)) = id
  | getId (Id (id, _))        = id

(* 1-1 mapping between ast ids (ints) and type vars (strings) *)
structure Env = BiMapFn(
   structure Key = struct
      type ord_key = String.string
      val compare = String.compare
   end
   structure Val = struct
      type ord_key = int
      val compare = Int.compare
   end)

(* constraint relates types to types *)
type constraint = {lhs: T.t, rhs: T.t}

structure ConstrSet = BinarySetFn(
   struct
      type ord_key = constraint
      val compare : constraint * constraint -> order =
          fn ({lhs = lhs , rhs = rhs },
              {lhs = lhs', rhs = rhs'}) =>
             case T.compare (lhs, lhs') of
                 EQUAL => T.compare (rhs, rhs')
               | ord => ord
   end)

   fun showAst (Bool (id, b)) = "Bool " ^ Int.toString id ^ "," ^ Bool.toString b
     | showAst (Num (id, n)) = "Num " ^ Int.toString id ^ "," ^ Int.toString n
     | showAst (Succ (id, e)) = "Succ (" ^ Int.toString id ^ "," ^ showAst e ^ ")"
     | showAst (Pred (id, e)) = "Pred (" ^ Int.toString id ^ "," ^ showAst e ^ ")"
     | showAst (IsZero (id, e)) = "IsZero (" ^ Int.toString id ^ "," ^ showAst e ^ ")"
     | showAst (If (id, e1, e2, e3)) =
       "If (" ^ Int.toString id ^ ","
       ^ showAst e1 ^ "," ^ showAst e2 ^ "," ^ showAst e3 ^ ")"
     | showAst (App (id, e1, e2)) = "App (" ^ Int.toString id ^ "," ^ showAst e1 ^ ","
                                                   ^ showAst e2 ^ ")"
     | showAst (Fun (id1, id2, x, e)) =
       "Fun (" ^ Int.toString id1 ^ "," ^ Int.toString id2 ^
       "," ^ x ^ "," ^ showAst e ^ ")"
     | showAst (Id (id, x)) = "Id (" ^ Int.toString id ^ "," ^ x ^ ")"

   structure ShowString =
      struct
         type t = string
         val show = Show.string
      end
   structure ShowAst =
      struct
         type t = ast
         val show = showAst
      end

   fun showConstr ({lhs, rhs} : constraint) =
       "{" ^ T.show lhs ^ "," ^ T.show rhs ^ "}"

   structure ShowConstraintSet =
      SetShowFn(structure Set = ConstrSet
                structure Show = struct
                   type t = constraint
                   val show = showConstr
                end)

local
   val tVarId = ref 0
   val letters = "abcdefghijklmnopqrstuvwxyz"
in
   fun gensym () =
       (Char.toString (String.sub (letters, !tVarId mod 26)) ^ Int.toString (!tVarId))
       before (tVarId := !tVarId + 1)
   fun reset () = tVarId := 0
end

local
   (* lookup an ast node in the env, return its tyvar, or generate a fresh one *)
   fun lookup (env, ast) =
       case Env.findV (env, getId ast) of
           SOME tvar => (tvar, env)
         | NONE =>
           let
              val tvar = gensym ()
           in
              (tvar, Env.insert (env, tvar, getId ast))
           end
in

type env = Env.map
val rec genCon : ast * ConstrSet.set * env -> ConstrSet.set * env =
 fn (e, constrs, env) =>
    let
       val (tvar, env') = lookup (env, e)

       fun builtin (self, child, arg, ret, cs, env) =
           let
              val (child', env') = lookup (env, child)
              val (self', env'') = lookup (env', self)
              val cs' = ConstrSet.addList (cs,
                                           [{lhs = T.Var self', rhs = ret},
                                            {lhs = T.Var child', rhs = arg}])
           in
              genCon (child, cs', env'')
           end

    in
       case e of

           (Bool _) => (ConstrSet.add (constrs, {lhs = T.Var tvar, rhs = T.Bool}), env')

         | (Num _) => (ConstrSet.add (constrs, {lhs = T.Var tvar, rhs = T.Num}), env')

         | Succ (_, e1) => builtin (e, e1, T.Num, T.Num, constrs, env')

         | Pred (_, e1) => builtin (e, e1, T.Num, T.Num, constrs, env')

         | IsZero (_, e1) => builtin (e, e1, T.Num, T.Bool, constrs, env')

         | If (_, e1, e2, e3) =>
           let
              fun f (x, (cs, env)) = genCon (x, cs, env)
              val (constrs', env'') = foldl f (constrs, env') [e1, e2, e3]
              val (tv1, env''') = lookup (env'', e1)
              val (tv2, env'''') = lookup (env''', e2)
              val (tv3, env''''') = lookup (env'''', e3)
              val constrs'' = [
                 {lhs = T.Var tv1, rhs = T.Bool},
                 {lhs = T.Var tv2, rhs = T.Var tv3},
                 {lhs = T.Var tv3, rhs = T.Var tv2},
                 {lhs = T.Var tvar, rhs = T.Var tv3}
              ]
           in
              (ConstrSet.addList (constrs', constrs''), env''''')
           end

         | Fun (boundid, _, var, body) =>
           let
              val (tvbody, env'') = lookup (env', body)
              val (tvid, env''') = lookup (env'', Id (boundid, var))
              val (constrs', env'''') = genCon (body, constrs, env''')
           in
              (ConstrSet.add (constrs', {lhs = T.Var tvar, rhs = T.Arrow (T.Var tvid, T.Var tvbody)}), env'''')
           end

         | Id (_, name) => (constrs, env')

         | App (_, f, a) =>
           let
              val (tvf, env'') = lookup (env', f)
              val (tva, env''') = lookup (env'', a)
              val (constrs', env'''') = genCon (f, constrs, env''')
              val (constrs'', env''''') = genCon (a, constrs', env'''')
           in
              (ConstrSet.add (constrs'',
                              {lhs = T.Var tvf, rhs = T.Arrow (T.Var tva, T.Var tvar)}),
               env''''')
           end

    end
end

exception NotImplemented of string
exception TypeError
exception Conflict
exception Assert of string

(* replace tv with typ in a single type *)
fun replace (_, _, T.Bool)            = T.Bool
  | replace (_, _, T.Num)             = T.Num
  | replace (tv, typ, t as T.Var tv') = if tv = tv' then typ else t
  | replace (tv, typ, T.Arrow (d, r)) =
    T.Arrow (replace (tv, typ, d), replace (tv, typ, r))

(* apply a substitution to a single type *)
fun substitute s T.Bool           = T.Bool
  | substitute s T.Num            = T.Num
  | substitute s (t as T.Var tv)  = (case StringMap.find (s, tv) of
                                      SOME t' => t'
                                    | NONE    => t)
  | substitute s (T.Arrow (d, r)) = T.Arrow (substitute s d, substitute s r)

(* replace tv with typ in a substitution *)
fun extend (tv : string, typ : T.t, s : T.t StringMap.map) : T.t StringMap.map =
    let
       fun f (k, v) =
           if k = tv
              then raise (Assert ("apply2S: bound type var in substitution, " ^
                          k ^ " bound to " ^ T.show v))
           else replace (tv, typ, v)
    in
       StringMap.insert (StringMap.mapi f s, tv, typ)
    end

fun substituteInC s ({lhs, rhs} : constraint) : constraint =
    {lhs = substitute s lhs, rhs = substitute s rhs}

val applyAndExtend : (T.t StringMap.map * constraint list * string * T.t) -> constraint list * T.t StringMap.map =
    fn (sub, stack, tv, typ) =>
       case StringMap.find (sub, tv) of
           (* unbound... *)
           NONE => let val sub' = extend (tv, typ, sub)
                   in (map (substituteInC sub') stack, sub')
                   end
         (* what to do if this is already bound ? *)
         | SOME _ => raise (NotImplemented "applyAndExtend")

fun unify (constrs : ConstrSet.set) =
    let
       fun unify' ([], acc) = acc
         | unify' ({lhs, rhs} :: stack, acc) =
           case (lhs, rhs) of
               (T.Var tv, typ) => unify' (applyAndExtend (acc, stack, tv, typ))

             | (typ, T.Var tv) => unify' (applyAndExtend (acc, stack, tv, typ))

             | (T.Arrow (dom, rng), T.Arrow (dom', rng')) =>
               unify' ({lhs = dom, rhs = dom'} :: {lhs = rng, rhs = rng'} :: stack, acc)

             | (T.Bool, T.Bool) => unify' (stack, acc)

             | (T.Num, T.Num) => unify' (stack, acc)

             | _ => raise TypeError
    in
       unify' (ConstrSet.listItems constrs, StringMap.empty)
    end


fun typeof (e : ast) : T.t =
    (reset ();
     let
        val (constraints, env) = genCon (e, ConstrSet.empty, Env.empty)
        val substitution = unify constraints
     in
        T.normalize (Option.valOf (StringMap.find (substitution,
                                      Option.valOf (Env.findV (env, getId e)))))
     end)

end
