structure TypeInf =
struct

datatype typ = TNum
             | TBool
             | TVar of string

(* boilerplate comparison stuff *)
fun typcompare (TNum       , TNum)                   = EQUAL
  | typcompare (TBool      , TBool)                  = EQUAL
  | typcompare (TVar s   , TVar s') = String.compare (s, s')
  | typcompare (TNum     , _)       = GREATER
  | typcompare (TBool    , TNum)    = LESS
  | typcompare (TBool    , _)       = GREATER
  | typcompare (TVar _   , _)       = LESS

datatype ast = Bool of bool
             | Num of int
             | Succ of ast
             | Pred of ast
             | IsZero of ast

(* boilerplate comparison stuff *)
fun astcompare (Num n      , Num n')     = Int.compare (n, n')
  | astcompare (Num _      , _)          = GREATER
  | astcompare (Bool true  , Bool false) = GREATER
  | astcompare (Bool false , Bool true)  = LESS
  | astcompare (Bool _     , Bool _)     = EQUAL
  | astcompare (Bool _     , Num _)      = LESS
  | astcompare (Bool _     ,  _)         = GREATER
  | astcompare (Succ e     , Succ e')    = astcompare (e, e')
  | astcompare (Succ _     , Num _)      = LESS
  | astcompare (Succ _     , Bool _)     = LESS
  | astcompare (Succ _     ,  _)         = GREATER
  | astcompare (Pred e     , Pred e')    = astcompare (e, e')
  | astcompare (Pred _     , Num _)      = LESS
  | astcompare (Pred _     , Bool _)     = LESS
  | astcompare (Pred _     , Succ _)     = LESS
  | astcompare (Pred _     , _)          = GREATER
  | astcompare (IsZero e   , IsZero e')  = astcompare (e, e')
  | astcompare (IsZero _   , Num _)      = LESS
  | astcompare (IsZero _   , Bool _)     = LESS
  | astcompare (IsZero _   , Succ _)     = LESS
  | astcompare (IsZero _   , Pred _)     = LESS

(*
 * Used to map type vars (string) to ast nodes (ast)
 * also used to map variables (string) to type vars (string)
 *)
structure StringMap = BinaryMapFn(
   struct
      type ord_key = String.string
      val compare = String.compare
   end)

structure AstMap = BinaryMapFn(
   struct
      type ord_key = ast
      val compare = astcompare
   end)

(* constraint is a mapping from type var (string) to type *)
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

local
   val astId = ref 0
   val tVarId = ref 0
   val letters = Array.fromList (String.explode "abcdefghijklmnopqrstuvwxyz")
in
   fun gensym () =
       let
          val tVarId' = !tVarId
       in
          (tVarId := tVarId' + 1
          ; Char.toString (Array.sub (letters, tVarId' mod 26)) ^ Int.toString tVarId')
       end
   fun newAstId () =
       let
          val astId' = !astId
       in
          (astId := astId' + 1
          ; astId')
       end
   fun reset () =
       (astId := 0
       ; tVarId := 0)
end

local
   fun getTVar (e, env as (tVar2Ast, ast2TVar)) =
       case AstMap.find (ast2TVar, e) of
           SOME tvar => (tvar, env)
         | NONE =>
           let
              val tvar = gensym ()
           in
              (tvar, (StringMap.insert (tVar2Ast, tvar, e),
                      AstMap.insert (ast2TVar, e, tvar)))
           end
in

type env = ast StringMap.map * string AstMap.map
val rec genCon : ast * ConstrSet.set * env -> ConstrSet.set * env =
 fn (e, constrs, env as (tVar2Ast, ast2TVar)) =>
    let
       val (tvar, env' as (tVar2Ast', ast2TVar')) = getTVar (e, env)
    in
       case e of

           (Bool _) => (ConstrSet.add (constrs, {lhs = tvar, rhs = TBool}), env')

         | (Num _) => (ConstrSet.add (constrs, {lhs = tvar, rhs = TNum}), env')

         | Succ e1 =>
           let
              val (constrs', env'' as (tVar2Ast'', ast2TVar'')) =
                  genCon (e1, constrs, env')
              val child = Option.valOf (AstMap.find (ast2TVar'', e1))
           in
              (ConstrSet.add (ConstrSet.add (constrs', {lhs = tvar, rhs = TNum}),
                              {lhs = child, rhs = TNum}),
               env'')
           end

         | Pred e1 => (* identical to Succ case above :( *)
           let
              val (constrs', env'' as (tVar2Ast'', ast2TVar'')) =
                  genCon (e1, constrs, env')
              val child = Option.valOf (AstMap.find (ast2TVar'', e1)) 
           in
              (ConstrSet.add (ConstrSet.add (constrs', {lhs = tvar, rhs = TNum}),
                              {lhs = child, rhs = TNum}),
               env'')
           end

         | IsZero e1 =>
           let
              val (constrs', env'' as (tVar2Ast'', ast2TVar'')) =
                  genCon (e1, constrs, env')
              val child = Option.valOf (AstMap.find (ast2TVar'', e1))
           in
              (ConstrSet.add (ConstrSet.add (constrs', {lhs = tvar, rhs = TBool}),
                              {lhs = child, rhs = TNum}),
               env'')
           end

    end
end

exception NotImplemented
exception Conflict

fun resolve ({lhs, rhs} : constr, subst) =
    case StringMap.find (subst, lhs) of
        SOME bound => (case (bound, rhs) of
                           (TNum, TNum) => subst
                         | (TBool, TBool) => subst
                         | (TVar _, TVar _) => raise NotImplemented
                         | _ => raise Conflict)
      | NONE => StringMap.insert (subst, lhs, rhs)

fun unify (constrs : ConstrSet.set) =
    let
       fun unify' ([], acc) = acc
         | unify' (c :: stack, acc) = unify' (stack, resolve (c, acc))
    in
       unify' (ConstrSet.listItems constrs, StringMap.empty)
    end

fun typeof e =
    (reset ();
     let
        val (constraints, (tVar2Ast, ast2TVar)) =
            genCon (e, ConstrSet.empty, (StringMap.empty, AstMap.empty))
        val substitution = unify constraints
     in
        Option.valOf (StringMap.find (substitution,
                                      Option.valOf (AstMap.find (ast2TVar, e))))
     end)

end
