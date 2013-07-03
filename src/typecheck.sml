structure Typecheck =
struct

structure T = Type
structure E = AST.Expr

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

type typed = {pos : AST.pos, typ: T.t}

structure Env = StringMap (* environment mapping vars in obj lang to types *)
exception Unbound (* thrown if there is an unbound ident *)

(*
 * Given an expression, assign each sub expression type vars unless it's an ident bound in the existing gamma
 *)
fun assignTypeVars (env : T.t Env.map, e : AST.pos E.t) : typed E.t =
    let
       fun makeTyped (p : AST.pos) : typed = {pos = p, typ = T.Var (gensym ())}
    in
       case e of
           E.Num (p, n) => E.Num (makeTyped p, n)
         | E.Bool (p, b) => E.Bool (makeTyped p, b)
         | E.Infix (p, bin, e1, e2) => E.Infix (makeTyped p, bin, assignTypeVars (env, e1), assignTypeVars (env, e2))
         | E.App (p, e1, e2) => E.App (makeTyped p, assignTypeVars (env, e1), assignTypeVars (env, e2))
         | E.If (p, e1, e2, e3) => E.If (makeTyped p, assignTypeVars (env, e1), assignTypeVars (env, e2), assignTypeVars (env, e3))

         | E.Fn (boundp, selfp, x, body) =>
           let
              val boundVar = gensym ()
           in
              E.Fn ({pos = boundp, typ = T.Var boundVar}, makeTyped selfp, x, assignTypeVars (Env.insert (env, x, T.Var boundVar), body))
           end

         | E.Id (p, x) =>
           (case Env.find (env, x) of
                SOME typ => E.Id ({pos = p, typ = typ}, x)
              | NONE => raise Unbound)

         | E.Tuple (p, es) => E.Tuple (makeTyped p, map (fn e => assignTypeVars (env, e)) es)
    end

fun gettyp (E.Num ({typ, ...} : typed, _))  = typ
  | gettyp (E.Bool ({typ, ...}, _))        = typ
  | gettyp (E.Id ({typ, ...}, _))          = typ
  | gettyp (E.Infix ({typ, ...}, _, _, _)) = typ
  | gettyp (E.App ({typ, ...}, _, _))      = typ
  | gettyp (E.If ({typ, ...}, _, _, _))    = typ
  | gettyp (E.Fn (_, {typ, ...}, _, _))    = typ

val rec genCon : (typed E.t * Constraint.Set.set) -> Constraint.Set.set =
 fn (e, constrs) =>
    let
       fun builtin (self, child1, child2, arg1, arg2, ret, cs) =
           let
              val child1' = gettyp child1
              val child2' = gettyp child2
              val self' = gettyp self
              val cs' = Constraint.Set.addList (cs,
                                           [{lhs = self', rhs = ret},
                                            {lhs = child1', rhs = arg1},
                                            {lhs = child2', rhs = arg2}])
              val cs'' = genCon (child1, cs')
              val cs''' = genCon (child2, cs'')
           in
              cs'''
           end

    in
       case e of

           E.Bool ({typ, ...}, _) => Constraint.Set.add (constrs, {lhs = typ, rhs = T.Bool})

         | E.Num ({typ, ...}, _) => Constraint.Set.add (constrs, {lhs = typ, rhs = T.Num})

         | E.If ({typ, ...}, e1, e2, e3) =>
           let
              fun f (x, cs) = genCon (x, cs)
              val constrs' = foldl f constrs [e1, e2, e3]
              val tv1 = gettyp e1
              val tv2 = gettyp e2
              val tv3 = gettyp e3
              val constrs'' = [
                 {lhs = tv1, rhs = T.Bool},
                 {lhs = tv2, rhs = typ},
                 {lhs = tv3, rhs = typ}
              ]
           in
              Constraint.Set.addList (constrs', constrs'')
           end

         | E.Fn ({typ=bound, ...}, {typ=self, ...}, x, body) =>
           let
              val tvbody = gettyp body
              val constrs' = genCon (body, constrs)
           in
              Constraint.Set.add (constrs', {lhs = self, rhs = T.Arrow (bound, tvbody)})
           end

         | E.Id _ => constrs

         | E.App ({typ,...}, f, a) =>
           let
              val tvf = gettyp f
              val tva = gettyp a
              val constrs' = genCon (f, constrs)
              val constrs'' = genCon (a, constrs')
           in
              Constraint.Set.add (constrs'',
                             {lhs = tvf, rhs = T.Arrow (tva, typ)})
           end

         | E.Tuple ({typ, ...}, es) =>
           let
              val constrs' = foldl genCon constrs es
           in
              Constraint.Set.add (constrs', {lhs = typ, rhs = T.Tuple (map gettyp es)})
           end

    end

fun prettyPrintConstraint ({lhs, rhs} : Constraint.t, env, ast) : string =
    let
       fun showTyp T.Num = "num"
         | showTyp T.Bool = "bool"
         | showTyp (T.Arrow (t1, t2)) = "(" ^ showTyp t1 ^ ") -> (" ^ showTyp t2 ^ ")"
         | showTyp (T.List t) = "[" ^ showTyp t ^ "]"
         | showTyp (T.Var tv) = tv
         | showTyp (T.Tuple ts) = "(" ^ String.concatWith "," (map showTyp ts) ^ ")"
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
  | replace (tv, typ, T.Arrow (d, r)) = T.Arrow (replace (tv, typ, d), replace (tv, typ, r))
  | replace (tv, typ, T.List t)       = T.List (replace (tv, typ, t))
  | replace (tv, typ, T.Tuple ts)     = T.Tuple (map (fn t => replace (tv, typ, t)) ts)

(* apply a substitution to a single type *)
fun substitute s T.Bool           = T.Bool
  | substitute s T.Num            = T.Num
  | substitute s (t as T.Var tv)  = (case StringMap.find (s, tv) of
                                      SOME t' => t'
                                    | NONE    => t)
  | substitute s (T.Arrow (d, r)) = T.Arrow (substitute s d, substitute s r)
  | substitute s (T.List t)       = T.List (substitute s t)
  | substitute s (T.Tuple ts) = T.Tuple (map (substitute s) ts)

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

fun substituteInC s ({lhs, rhs} : Constraint.t) : Constraint.t =
    {lhs = substitute s lhs, rhs = substitute s rhs}

exception VarBoundInSub
val applyAndExtend : (T.t StringMap.map * Constraint.t list * string * T.t) -> Constraint.t list * T.t StringMap.map =
    fn (sub, stack, tv, typ) =>
       case StringMap.find (sub, tv) of
           (* unbound... *)
           NONE => let val sub' = extend (tv, typ, sub)
                   in (map (substituteInC sub') stack, sub')
                   end
         (* I think this should never happen... *)
         | SOME _ => raise VarBoundInSub

exception Occurs
fun occurs ({lhs, rhs} : Constraint.t) : bool =
    let
       fun occurs' (tv, T.Num) = false
         | occurs' (tv, T.Bool) = false
         | occurs' (tv, T.Var tv') = tv = tv'
         | occurs' (tv, T.List ty) = occurs' (tv, ty)
         | occurs' (tv, T.Arrow (t1, t2)) = occurs' (tv, t1) orelse occurs' (tv, t2)
         | occurs' (tv, T.Tuple ts) = List.exists (fn t => occurs' (tv, t)) ts
    in
       case (lhs, rhs) of
           (* constraint that just relates any two vars is ok *)
           (T.Var _, T.Var _) => false
         | (T.Var tv, ty) => occurs' (tv, ty)
         | (ty, T.Var tv) => occurs' (tv, ty)
         | _ => false
    end


fun unify (constrs : Constraint.Set.set) =
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

                | (T.Tuple ts, T.Tuple ts') => unify' ((map (fn (t, t') => {lhs = t, rhs = t'}) (ListPair.zip (ts, ts'))) @ stack, acc)

                | _ => raise TypeError)
    in
       unify' (Constraint.Set.listItems constrs, StringMap.empty)
    end

fun applySubToAST (e : typed E.t, sub : T.t StringMap.map) =
    let
       fun getType (tv, sub) =
           case StringMap.find (sub, tv) of
               SOME ty => ty
             | NONE => T.Var tv
    in
       case e of
           E.Num ({typ = T.Var tv, pos}, n) => E.Num ({typ = getType (tv, sub), pos = pos}, n)
         | E.Num _ => e

         | E.Bool ({typ = T.Var tv, pos}, b) => E.Bool ({typ = getType (tv, sub), pos=pos}, b)
         | E.Bool _ => e

         | E.Id ({typ = T.Var tv, pos}, x) => E.Id ({typ = getType (tv, sub), pos = pos}, x)
         | E.Id _ => e

         | E.App ({typ = T.Var tv, pos}, f, a) => E.App ({typ = getType (tv, sub), pos = pos}, applySubToAST (f, sub), applySubToAST (a, sub))
         | E.App _ => e

         | E.If ({typ = T.Var tv, pos}, e1, e2, e3) => E.If ({typ=getType (tv, sub), pos=pos}, applySubToAST (e1, sub), applySubToAST (e2, sub), applySubToAST (e3, sub))
         | E.If _ => e

         | E.Fn ({typ = T.Var tv1, pos = p1}, {typ = T.Var tv2, pos = p2}, x, e) => E.Fn ({typ = getType (tv1, sub), pos = p1},
                                                                                          {typ = getType (tv2, sub), pos=p2}, x, applySubToAST (e, sub))
         | E.Fn _ => e

         | E.Tuple ({typ = T.Var tv, pos}, es) => E.Tuple ({typ = getType (tv, sub), pos = pos}, map (fn e => applySubToAST (e, sub)) es)
         | E.Tuple (typed, es) => E.Tuple (typed, map (fn e => applySubToAST (e, sub)) es)
    end

fun typeof (e : AST.pos E.t) =
    (reset ();
     let
        val ast = assignTypeVars (Env.empty, e)
        val constraints = genCon (ast, Constraint.Set.empty)
        val substitution = unify constraints
     in
        applySubToAST (ast, substitution)
     end)

end
