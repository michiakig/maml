structure TypeInf =
struct

datatype typ = TNum
             | TBool
             | TArrow of typ * typ
             | TVar of string

(* boilerplate comparison stuff *)
fun typcompare (TNum       , TNum)                   = EQUAL
  | typcompare (TBool      , TBool)                  = EQUAL
  | typcompare (TArrow (t1 , t2), TArrow (t1', t2')) =
    (case typcompare (t1, t1') of
        EQUAL => typcompare (t2', t2')
      | ord => ord)
  | typcompare (TVar s   , TVar s') = String.compare (s, s')
  | typcompare (TNum     , _)       = GREATER
  | typcompare (TBool    , TNum)    = LESS
  | typcompare (TBool    , _)       = GREATER
  | typcompare (TArrow _ , TNum)    = LESS
  | typcompare (TArrow _ , TBool)   = LESS
  | typcompare (TArrow _ , _)       = GREATER
  | typcompare (TVar _   , _)       = LESS

datatype ast = Num of int * int
             | Bool of int * bool
             | Succ of int * ast
             | Pred of int * ast
             | IsZero of int * ast
             | If of int * ast * ast * ast
             | App of int * ast * ast
             | Fun of int * string * ast
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
  | findByid (n as Fun (id, _, e), id') =
    if id = id'
       then SOME n
    else (case findByid (e, id') of
              SOME n => SOME n
            | NONE => NONE)
  | findByid (n as Id (id, _), id') = if id = id' then SOME n else NONE

fun getId (Num (id, _))      = id
  | getId (Bool (id, _))     = id
  | getId (Succ (id, _))     = id
  | getId (Pred (id, _))     = id
  | getId (IsZero (id, _))   = id
  | getId (If (id, _, _, _)) = id
  | getId (App (id, _, _))   = id
  | getId (Fun (id, _, _))   = id
  | getId (Id (id, _))       = id

(* substitution, map from typvars (strings) to typ *)
structure StringMap = BinaryMapFn(
   struct
      type ord_key = String.string
      val compare = String.compare
   end)

(* environment, 1-1 mapping between ast ids (ints) and type vars (strings) *)
structure Env = BiMapFn(
   structure Key = struct
      type ord_key = String.string
      val compare = String.compare
   end
   structure Val = struct
      type ord_key = int
      val compare = Int.compare
   end)

(* constraint is relates a type var (string) to type *)
type constr = {lhs: string, rhs: typ}

structure ConstrSet = BinarySetFn(
   struct
      type ord_key = constr
      val compare : constr * constr -> order =
          fn ({lhs = lhs , rhs = rhs },
              {lhs = lhs', rhs = rhs'}) =>
             let
                val ord = String.compare (lhs, lhs')
             in
                case ord of
                    EQUAL => typcompare (rhs, rhs')
                  | _ => ord
             end
   end)

   fun showTyp TNum     = "TNum"
     | showTyp TBool    = "TBool"
     | showTyp (TVar s) = "TVar " ^ s
     | showTyp (TArrow (t1, t2)) =
       "TArrow (" ^ showTyp t1 ^ "," ^ showTyp t2 ^ "," ^ ")"

   fun showAst (Bool    (_, b))          = "Bool " ^ Bool.toString b
     | showAst (Num     (_, n))          = "Num " ^ Int.toString n
     | showAst (Succ    (_, e))          = "Succ (" ^ showAst e  ^ ")"
     | showAst (Pred    (_, e))          = "Pred (" ^ showAst e  ^ ")"
     | showAst (IsZero  (_, e))          = "IsZero (" ^ showAst e  ^ ")"
     | showAst (If      (_, e1, e2, e3)) = "If (" ^ showAst e1 ^ ","
                                                  ^ showAst e2 ^ ","
                                                  ^ showAst e3 ^ ")"
     | showAst (App     (_, e1, e2))     = "App (" ^ showAst e1 ^ ","
                                                   ^ showAst e2 ^ ")"
     | showAst (Fun     (_, x, e))       = "Fun (" ^ x ^ "," ^ showAst e ^ ")"
     | showAst (Id      (_, x))          = "Id " ^ x

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

   fun showConstr ({lhs, rhs} : constr) = "{" ^ lhs ^ "," ^ showTyp rhs ^ "}"

   structure ShowConstraintSet =
      SetShowFn(structure Set = ConstrSet
                structure Show = struct
                   type t = constr
                   val show = showConstr
                end)

local
   val tVarId = ref 0
   val letters = Array.fromList (String.explode "abcdefghijklmnopqrstuvwxyz")
in
   fun gensym () =
       (Char.toString (Array.sub (letters, !tVarId mod 26)) ^ Int.toString (!tVarId))
       before (tVarId := !tVarId + 1)
   fun reset () = tVarId := 0
end

exception Bound
local
   fun lookup (e, env) =
       case Env.findV (env, getId e) of
           SOME tvar => (tvar, env)
         | NONE =>
           let
              val tvar = gensym ()
           in
              (tvar, Env.insert (env, tvar, getId e))
           end
   fun insert (e, tvar, env) =
       case Env.findV (env, getId e) of
           NONE => Env.insert (env, tvar, getId e)
         | _ => raise Bound
in

type env = Env.map
val rec genCon : ast * ConstrSet.set * env -> ConstrSet.set * env =
 fn (e, constrs, env) =>
    let
       val (tvar, env') = lookup (e, env)
    in
       case e of

           (Bool _) => (ConstrSet.add (constrs, {lhs = tvar, rhs = TBool}), env')

         | (Num _) => (ConstrSet.add (constrs, {lhs = tvar, rhs = TNum}), env')

         | Succ (_, e1) =>
           let
              val (constrs', env'') = genCon (e1, constrs, env')
              val child = Option.valOf (Env.findV (env'', getId e1))
           in
              (ConstrSet.add (ConstrSet.add (constrs', {lhs = tvar, rhs = TNum}),
                              {lhs = child, rhs = TNum}),
               env'')
           end

         | Pred (_, e1) => (* identical to Succ case above :( *)
           let
              val (constrs', env'') = genCon (e1, constrs, env')
              val child = Option.valOf (Env.findV (env'', getId e1))
           in
              (ConstrSet.add (ConstrSet.add (constrs', {lhs = tvar, rhs = TNum}),
                              {lhs = child, rhs = TNum}),
               env'')
           end

         | IsZero (_, e1) =>
           let
              val (constrs', env'') = genCon (e1, constrs, env')
              val child = Option.valOf (Env.findV (env'', getId e1))
           in
              (ConstrSet.add (ConstrSet.add (constrs', {lhs = tvar, rhs = TBool}),
                              {lhs = child, rhs = TNum}),
               env'')
           end

         | If (_, e1, e2, e3) =>
           let
              val tv1 = gensym ()
              val tv2 = gensym ()
              val tv3 = gensym ()
              val (constrs', env'') =
                  genCon (e1, constrs,
                          insert (e3, tv3,
                                  insert (e2, tv2,
                                          insert (e1, tv1, env'))))
              val (constrs'', env''') = genCon (e2, constrs', env'')
              val (constrs''', env'''') = genCon (e3, constrs'', env''')
              val constrs = [
                 {lhs = tv1, rhs = TBool},
                 {lhs = tv2, rhs = TVar tv3},
                 {lhs = tv3, rhs = TVar tv2},
                 {lhs = tvar, rhs = TVar tv3}
              ]
           in
              (ConstrSet.addList (constrs''', constrs), env'''')
           end

    end
end

exception NotImplemented of string
exception TypeError
exception Conflict

(* apply a substitution to a single type *)
fun applySub2Typ (s, TBool)           = TBool
  | applySub2Typ (s, TNum)            = TNum
  | applySub2Typ (s, typ as TVar tv)  =
    (case StringMap.find (s, tv) of
         SOME typ' => typ'
       | NONE      => typ)
  | applySub2Typ (s, TArrow (t1, t2)) = TArrow (applySub2Typ (s, t1),
                                                applySub2Typ (s, t2))

(* apply a substitution to a constraint *)
fun applyS2C (s, c as {lhs, rhs} : constr) : constr option =
    let
       val rhs' = applySub2Typ (s, rhs)
    in
       case (StringMap.find (s, lhs), rhs') of
           (SOME (TVar b), _)  => SOME {lhs = b, rhs = rhs' }
         | (SOME typ, TVar tv) => SOME {lhs = tv, rhs = applySub2Typ (s, typ)}
         | (SOME TBool, TBool) => NONE
         | (SOME TNum, TNum)   => NONE
         | (SOME TBool, TNum)  => raise TypeError
         | (SOME TNum, TBool)  => raise TypeError
         | (NONE, _)           => SOME {lhs = lhs, rhs = rhs'}
         | _                   => raise NotImplemented "applyS2C"
    end

(* apply a constraint to a substitution *)
fun applyC2S (c as {lhs, rhs}, s) : typ StringMap.map =
    let
       fun f (k, v) =
           case v of
               (TVar v') => if lhs = v' then rhs else v
             | _ => v
    in
       StringMap.insert (StringMap.mapi f s, lhs, rhs)
    end

fun extendSub (c as {lhs, rhs} : constr, s) : typ StringMap.map =
    case applyS2C (s, c) of
        SOME (c' as {lhs, rhs}) => applyC2S (c', StringMap.insert (s, lhs, rhs))
      | NONE => s

fun unify (constrs : ConstrSet.set) =
    let
       fun unify' ([], acc) = acc
         | unify' (c :: stack, acc) = unify' (stack, extendSub (c, acc))
    in
       unify' (ConstrSet.listItems constrs, StringMap.empty)
    end

fun typeof (e : ast) : typ =
    (reset ();
     let
        val (constraints, env) = genCon (e, ConstrSet.empty, Env.empty)
        val substitution = unify constraints
     in
        Option.valOf (StringMap.find (substitution,
                                      Option.valOf (Env.findV (env, getId e))))
     end)

end
