structure TypeInf =
struct

structure T = Type
structure E = AST.Expr

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
   (* generate fresh type variables *)
   fun gensym () =
       (Char.toString (String.sub (letters, !tVarId mod 26)) ^ Int.toString (!tVarId))
       before (tVarId := !tVarId + 1)
   fun reset () = tVarId := 0
end

type info = {pos : AST.pos, tv : string}
exception FreeVariable
local
   (* map from identifiers in the obj lang to type variable *)
   structure Env = BinaryMapFn(
      struct
         type ord_key = string
         val compare = String.compare
      end)
in
   fun assignTypeVars (ast : AST.pos E.t) : info E.t =
       let
          fun g (p : AST.pos) : info = {pos = p, tv = gensym ()}

          fun a (_, E.Num (pos, n)) = E.Num (g pos, n)
            | a (_, E.Bool (pos, b)) = E.Bool (g pos, b)
            | a (env, E.Infix (pos, bin, e1, e2)) = E.Infix (g pos, bin, a (env, e1), a (env, e2))

            | a (env, E.App (pos, e1, e2)) = E.App (g pos, a (env, e1), a (env, e2))
            | a (env, E.If (pos, e1, e2, e3)) = E.If (g pos, a (env, e1), a (env, e2), a (env, e3))

            | a (env, E.Fn (bound, self, x, e)) =
              let
                 val tv = gensym ()
              in
                 E.Fn ({pos=bound, tv = tv}, g self, x, a (Env.insert (env, x, tv), e))
              end

            | a (env, E.Id (pos, id)) =
              case Env.find (env, id) of
                  SOME tv => E.Id ({pos=pos, tv=tv}, id)
                | NONE => raise FreeVariable
       in
          a (Env.empty, ast)
       end
end

fun gettv (E.Num ({tv, ...} : info, _))  = tv
  | gettv (E.Bool ({tv, ...}, _))        = tv
  | gettv (E.Id ({tv, ...}, _))          = tv
  | gettv (E.Infix ({tv, ...}, _, _, _)) = tv
  | gettv (E.App ({tv, ...}, _, _))      = tv
  | gettv (E.If ({tv, ...}, _, _, _))    = tv
  | gettv (E.Fn (_, {tv, ...}, _, _))    = tv

val rec genCon : (info E.t * ConstrSet.set) -> ConstrSet.set =
 fn (e, constrs) =>
    let
       fun builtin (self, child1, child2, arg1, arg2, ret, cs) =
           let
              val child1' = gettv child1
              val child2' = gettv child2
              val self' = gettv self
              val cs' = ConstrSet.addList (cs,
                                           [{lhs = T.Var self', rhs = ret},
                                            {lhs = T.Var child1', rhs = arg1},
                                            {lhs = T.Var child2', rhs = arg2}])
              val cs'' = genCon (child1, cs')
              val cs''' = genCon (child2, cs'')
           in
              cs'''
           end

    in
       case e of

           E.Bool ({tv, ...}, _) => ConstrSet.add (constrs, {lhs = T.Var tv, rhs = T.Bool})

         | E.Num ({tv, ...}, _) => ConstrSet.add (constrs, {lhs = T.Var tv, rhs = T.Num})

         | E.If ({tv, ...}, e1, e2, e3) =>
           let
              fun f (x, cs) = genCon (x, cs)
              val constrs' = foldl f constrs [e1, e2, e3]
              val tv1 = gettv e1
              val tv2 = gettv e2
              val tv3 = gettv e3
              val constrs'' = [
                 {lhs = T.Var tv1, rhs = T.Bool},
                 {lhs = T.Var tv2, rhs = T.Var tv},
                 {lhs = T.Var tv3, rhs = T.Var tv}
              ]
           in
              ConstrSet.addList (constrs', constrs'')
           end

         | E.Fn ({tv=bound, ...}, {tv=self, ...}, x, body) =>
           let
              val tvbody = gettv body
              val constrs' = genCon (body, constrs)
           in
              ConstrSet.add (constrs', {lhs = T.Var self, rhs = T.Arrow (T.Var bound, T.Var tvbody)})
           end

         | E.Id _ => constrs

         | E.App ({tv,...}, f, a) =>
           let
              val tvf = gettv f
              val tva = gettv a
              val constrs' = genCon (f, constrs)
              val constrs'' = genCon (a, constrs')
           in
              ConstrSet.add (constrs'',
                             {lhs = T.Var tvf, rhs = T.Arrow (T.Var tva, T.Var tv)})
           end
    end

fun prettyPrintConstraint ({lhs, rhs} : constraint, env, ast) : string =
    let
       fun showTyp T.Num = "num"
         | showTyp T.Bool = "bool"
         | showTyp (T.Arrow (t1, t2)) = "(" ^ showTyp t1 ^ ") -> (" ^ showTyp t2 ^ ")"
         | showTyp (T.List t) = "[" ^ showTyp t ^ "]"
         | showTyp (T.Var tv) = tv
           (* (* lookup ty vars in env, replace with ast if possible *) *)
           (* (case Env.findK (env, tv) of *)
           (*      SOME id => (case E.findById (ast, id) of *)
           (*                      SOME n => "{(" ^ tv ^ ") " ^ E.show n ^ "}" *)
           (*                    | NONE => "?" ^ tv) *)
           (*    | NONE => tv) *)
    in
       showTyp lhs ^ " === " ^ showTyp rhs
    end

fun printConstraint (c, env, ast) = print (prettyPrintConstraint (c, env, ast) ^ "\n")

fun printConstraints (cs, env, ast) =
    List.app (fn c => printConstraint (c, env, ast)) cs

fun printSub s =
    List.app (fn (k, v) => print (k ^ ":=" ^ T.show v ^ "\n")) (StringMap.listItemsi s)

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
  | replace (tv, typ, T.List t)       = T.List (replace (tv, typ, t))

(* apply a substitution to a single type *)
fun substitute s T.Bool           = T.Bool
  | substitute s T.Num            = T.Num
  | substitute s (t as T.Var tv)  = (case StringMap.find (s, tv) of
                                      SOME t' => t'
                                    | NONE    => t)
  | substitute s (T.Arrow (d, r)) = T.Arrow (substitute s d, substitute s r)
  | substitute s (T.List t)       = T.List (substitute s t)

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

exception VarBoundInSub
val applyAndExtend : (T.t StringMap.map * constraint list * string * T.t) -> constraint list * T.t StringMap.map =
    fn (sub, stack, tv, typ) =>
       case StringMap.find (sub, tv) of
           (* unbound... *)
           NONE => let val sub' = extend (tv, typ, sub)
                   in (map (substituteInC sub') stack, sub')
                   end
         (* I think this should never happen... *)
         | SOME _ => raise VarBoundInSub

exception Occurs
fun occurs ({lhs, rhs} : constraint) : bool =
    let
       fun occurs' (tv, T.Num) = false
         | occurs' (tv, T.Bool) = false
         | occurs' (tv, T.Var tv') = tv = tv'
         | occurs' (tv, T.List ty) = occurs' (tv, ty)
         | occurs' (tv, T.Arrow (t1, t2)) = occurs' (tv, t1) orelse occurs' (tv, t2)
    in
       case (lhs, rhs) of
           (* constraint that just relates any two vars is ok *)
           (T.Var _, T.Var _) => false
         | (T.Var tv, ty) => occurs' (tv, ty)
         | (ty, T.Var tv) => occurs' (tv, ty)
         | _ => false
    end


fun unify (constrs : ConstrSet.set) =
    let
       fun unify' ([], acc) = acc
         | unify' ((c as {lhs, rhs}) :: stack, acc) =
           (
           if occurs c
              then raise Occurs
           else
              case (lhs, rhs) of
                  (T.Var tv1, t2 as T.Var tv2) =>
                  if tv1 = tv2
                     then unify' (stack, acc)
                  else unify' (applyAndExtend (acc, stack, tv1, t2))

                | (T.Var tv, typ) => unify' (applyAndExtend (acc, stack, tv, typ))

                | (typ, T.Var tv) => unify' (applyAndExtend (acc, stack, tv, typ))

                | (T.Arrow (dom, rng), T.Arrow (dom', rng')) =>
                  unify' ({lhs = dom, rhs = dom'} ::
                          {lhs = rng, rhs = rng'} :: stack, acc)

                | (T.Bool, T.Bool) => unify' (stack, acc)

                | (T.Num, T.Num) => unify' (stack, acc)

                | (T.List t, T.List t') => unify' ({lhs = t, rhs = t'} :: stack, acc)

                | _ => raise TypeError)
    in
       unify' (ConstrSet.listItems constrs, StringMap.empty)
    end


fun typeof (e : AST.pos E.t) : T.t =
    (reset ();
     let
        val ast = assignTypeVars e
        val constraints = genCon (ast, ConstrSet.empty)
        val substitution = unify constraints
        val ty = Option.valOf (StringMap.find (substitution, gettv ast))
     in
        T.normalize ty
     end)

end
