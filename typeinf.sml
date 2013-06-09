structure TypeInf =
struct

structure T = Type
structure A = Syntax.Abstract

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


   structure ShowString =
      struct
         type t = string
         val show = Show.string
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
       case Env.findV (env, A.getId ast) of
           SOME tvar => (tvar, env)
         | NONE =>
           let
              val tvar = gensym ()
           in
              (tvar, Env.insert (env, tvar, A.getId ast))
           end
in

type env = Env.map
val rec genCon : A.t * ConstrSet.set * env -> ConstrSet.set * env =
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

           (A.Bool _) => (ConstrSet.add (constrs, {lhs = T.Var tvar, rhs = T.Bool}), env')

         | (A.Num _) => (ConstrSet.add (constrs, {lhs = T.Var tvar, rhs = T.Num}), env')

         | A.Succ (_, e1) => builtin (e, e1, T.Num, T.Num, constrs, env')

         | A.Pred (_, e1) => builtin (e, e1, T.Num, T.Num, constrs, env')

         | A.IsZero (_, e1) => builtin (e, e1, T.Num, T.Bool, constrs, env')

         | A.If (_, e1, e2, e3) =>
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

         | A.Fun (boundid, _, var, body) =>
           let
              val (tvbody, env'') = lookup (env', body)
              val (tvid, env''') = lookup (env'', A.Id (boundid, var))
              val (constrs', env'''') = genCon (body, constrs, env''')
           in
              (ConstrSet.add (constrs', {lhs = T.Var tvar, rhs = T.Arrow (T.Var tvid, T.Var tvbody)}), env'''')
           end

         | A.Id (_, name) => (constrs, env')

         | A.App (_, f, a) =>
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


fun typeof (e : A.t) : T.t =
    (reset ();
     let
        val (constraints, env) = genCon (e, ConstrSet.empty, Env.empty)
        val substitution = unify constraints
     in
        T.normalize (Option.valOf (StringMap.find (substitution,
                                      Option.valOf (Env.findV (env, A.getId e)))))
     end)

end
