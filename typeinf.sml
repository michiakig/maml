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

(* boilerplate comparison stuff *)
fun astcompare (Num (_, n) , Num (_, n'))                  = Int.compare (n, n')
  | astcompare (Num _ , _)                                 = GREATER
  | astcompare (Bool (_, true), Bool (_, false))           = GREATER
  | astcompare (Bool (_, false), Bool (_, true))           = LESS
  | astcompare (Bool _ , Bool _)                           = EQUAL
  | astcompare (Bool _ , Num _)                            = LESS
  | astcompare (Bool _ ,  _)                               = GREATER
  | astcompare (Succ (_, e) , Succ (_, e'))                = astcompare (e, e')
  | astcompare (Succ _ , Num _)                            = LESS
  | astcompare (Succ _ , Bool _)                           = LESS
  | astcompare (Succ _ ,  _)                               = GREATER
  | astcompare (Pred (_, e) , Pred (_, e'))                = astcompare (e, e')
  | astcompare (Pred _ , Num _)                            = LESS
  | astcompare (Pred _ , Bool _)                           = LESS
  | astcompare (Pred _ , Succ _)                           = LESS
  | astcompare (Pred _ , _)                                = GREATER
  | astcompare (IsZero (_, e) , IsZero (_, e'))            = astcompare (e, e')
  | astcompare (IsZero _ , Num _)                          = LESS
  | astcompare (IsZero _ , Bool _)                         = LESS
  | astcompare (IsZero _ , Succ _)                         = LESS
  | astcompare (IsZero _ , Pred _)                         = LESS
  | astcompare (IsZero _ ,  _)                             = GREATER
  | astcompare (If (_, e1, e2, e3), If (_, e1', e2', e3')) =
    (case (astcompare (e1, e1'), astcompare (e2, e2'), astcompare (e3, e3')) of
         (EQUAL, EQUAL, ord) => ord
       | (EQUAL, ord, _)     => ord
       | (ord, _, _)         => ord)
  | astcompare (If _, App _)                               = GREATER
  | astcompare (If _, Fun _)                               = GREATER
  | astcompare (If _, Id _)                                = GREATER
  | astcompare (If _, _)                                   = LESS
  | astcompare (App (_, e1, e2), App (_, e1', e2'))        =
    (case (astcompare (e1, e1'), astcompare (e2, e2')) of
         (EQUAL, ord) => ord
       | (GREATER, _) => GREATER
       | (LESS, _)    => LESS)
  | astcompare (App _, Fun _)                              = GREATER
  | astcompare (App _, Id _)                               = GREATER
  | astcompare (App _, _)                                  = LESS
  | astcompare (Fun (_, s, e), Fun (_, s', e')) =
    (case String.compare (s, s') of
         EQUAL => astcompare (e, e')
       | ord   => ord)
  | astcompare (Fun _, Id _)                               = GREATER
  | astcompare (Fun _, _)                                  = LESS
  | astcompare (Id (_, i), Id (_, i'))                     = String.compare (i, i')
  | astcompare (Id _, _)                                   = LESS

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
   structure ShowAstStringMap =
      MapShowFn(structure Map = StringMap
                structure K = ShowString
                structure V = ShowAst)

   structure ShowStringAstMap =
      MapShowFn(structure Map = AstMap
                structure K = ShowAst
                structure V = ShowString)

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
       let
          val tVarId' = !tVarId
       in
          (tVarId := tVarId' + 1
          ; Char.toString (Array.sub (letters, tVarId' mod 26)) ^ Int.toString tVarId')
       end
   fun reset () = tVarId := 0
end

exception Bound
local
   fun lookup (e, env as (tVar2Ast, ast2TVar)) =
       case AstMap.find (ast2TVar, e) of
           SOME tvar => (tvar, env)
         | NONE =>
           let
              val tvar = gensym ()
           in
              (tvar, (StringMap.insert (tVar2Ast, tvar, e),
                      AstMap.insert (ast2TVar, e, tvar)))
           end
   fun insert (e, tvar, env as (tVar2Ast, ast2TVar)) =
       case (AstMap.find (ast2TVar, e), StringMap.find (tVar2Ast, tvar)) of
           (NONE, NONE) => (StringMap.insert (tVar2Ast, tvar, e),
                            AstMap.insert (ast2TVar, e, tvar))
         | _ => raise Bound
in

type env = ast StringMap.map * string AstMap.map
val rec genCon : ast * ConstrSet.set * env -> ConstrSet.set * env =
 fn (e, constrs, env as (tVar2Ast, ast2TVar)) =>
    let
       val (tvar, env' as (tVar2Ast', ast2TVar')) = lookup (e, env)
    in
       case e of

           (Bool _) => (ConstrSet.add (constrs, {lhs = tvar, rhs = TBool}), env')

         | (Num _) => (ConstrSet.add (constrs, {lhs = tvar, rhs = TNum}), env')

         | Succ (_, e1) =>
           let
              val (constrs', env'' as (tVar2Ast'', ast2TVar'')) =
                  genCon (e1, constrs, env')
              val child = Option.valOf (AstMap.find (ast2TVar'', e1))
           in
              (ConstrSet.add (ConstrSet.add (constrs', {lhs = tvar, rhs = TNum}),
                              {lhs = child, rhs = TNum}),
               env'')
           end

         | Pred (_, e1) => (* identical to Succ case above :( *)
           let
              val (constrs', env'' as (tVar2Ast'', ast2TVar'')) =
                  genCon (e1, constrs, env')
              val child = Option.valOf (AstMap.find (ast2TVar'', e1)) 
           in
              (ConstrSet.add (ConstrSet.add (constrs', {lhs = tvar, rhs = TNum}),
                              {lhs = child, rhs = TNum}),
               env'')
           end

         | IsZero (_, e1) =>
           let
              val (constrs', env'' as (tVar2Ast'', ast2TVar'')) =
                  genCon (e1, constrs, env')
              val child = Option.valOf (AstMap.find (ast2TVar'', e1))
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
              (ConstrSet.addList (constrs''', constrs),
               env'''')
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
        val (constraints, (tVar2Ast, ast2TVar)) =
            genCon (e, ConstrSet.empty, (StringMap.empty, AstMap.empty))
        val substitution = unify constraints
     in
        Option.valOf (StringMap.find (substitution,
                                      Option.valOf (AstMap.find (ast2TVar, e))))
     end)

end
