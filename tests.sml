structure TypeInfTests =
struct

structure T = Type
structure S = Syntax.Concrete
structure A = Syntax.Abstract

val typ = {show = T.show}
val ast = {show = A.show}

local
   fun fst (a, _) = a
   val t = TypeInf.typeof o A.makeAst
in
val typeof = Test.group ("typeof", Test.polyAssertEq typ,
[
  {expected = T.Num, actual = t (S.Num 0)}
 ,{expected = T.Num, actual = t (S.Succ (S.Num 0))}
 ,{expected = T.Num, actual = t (S.Pred (S.Num 0))}
 ,{expected = T.Bool, actual = t (S.IsZero (S.Num 0))}
 ,{expected = T.Bool, actual = t (S.IsZero (S.Pred (S.Num 0)))}
 ,{expected = T.Num, actual = t (S.If (S.Bool true, S.Num 0, S.Num 1))}
 ,{expected = T.Bool, actual = t (S.If (S.IsZero (S.Num 0), S.Bool true, S.Bool false))}
 ,{expected = T.Bool, actual = t (S.If (S.IsZero (S.Num 0), S.Bool true, S.Bool true))}
 ,{expected = T.Num, actual = t (S.If (S.IsZero (S.Num 0), S.Num 0, S.Num 0))}

 ,{expected = T.Arrow (T.Num, T.Bool), actual = t (S.Fun ("x", S.IsZero (S.Id "x")))}
 ,{expected = T.Arrow (T.Bool, T.Num),
   actual = t (S.Fun ("x", S.If (S.Id "x", S.Num 0, S.Num 1)))}

 ,{expected = T.Num, actual = t (S.App (S.Fun ("x", S.Id "x"), S.Num 0))}
 ,{expected = T.Bool, actual = t (S.App (S.Fun ("x", S.IsZero (S.Id "x")), S.Num 0))}

 ,{expected = T.Arrow (T.Var "a", T.Arrow (T.Var "b", T.Var "b")),
   actual = t (S.Fun ("x", S.Fun ("y", S.Id "y")))}

 ,{expected = T.List T.Num, actual = t (S.Cons (S.Num 0, S.Nil))}
 ,{expected = T.List T.Num, actual = t (S.Tl (S.Cons (S.Num 0, S.Nil)))}
 ,{expected = T.Num, actual = t (S.Hd (S.Cons (S.Num 0, S.Nil)))}
 ,{expected = T.Bool, actual = t (S.IsNil (S.Cons (S.Num 0, S.Nil)))}
 ,{expected = T.Arrow (T.Var "a", T.List (T.Var "a")),
   actual = t (S.Fun ("x", S.Cons (S.Id "x", S.Nil)))}
])

(* mostly testing to make sure bound vars get the correct ids *)
val m = fn e => (A.reset (); A.makeAst e)
val makeAst = (Test.group ("makeAst", Test.polyAssertEq ast,
[
  {expected = A.Num (0, 1), actual = m (S.Num 1)}
 ,{expected = A.Fun (0, 1, "x", A.Id (0, "x")), actual = m (S.Fun ("x", S.Id "x"))}
 ,{expected = A.Fun (0, 5, "x", A.If (1, A.Id (0, "x"), A.Fun (2, 3, "x", A.Id (2, "x")), A.Num (4, 0))),
   actual = m (S.Fun ("x", S.If (S.Id "x", S.Fun ("x", S.Id "x"), S.Num 0)))}
]))

val oc = TypeInf.occurs
val occurs = (Test.group ("occurs", Test.polyAssertEq {show = Show.bool},
[
  {expected = false, actual = oc {lhs = T.Var "x", rhs = T.Var "x"}}
, {expected = true, actual = oc {lhs = T.Var "x", rhs = T.List (T.Var "x")}}
, {expected = true, actual = oc {lhs = T.List (T.Var "x"), rhs = T.Var "x"}}
, {expected = true, actual = oc {lhs = T.Var "x", rhs = T.Arrow (T.Num, T.Var "x")}}
]))

end

fun main _ = (
Test.runTestSuite (true, Test.concat [typeof, makeAst, occurs]);
OS.Process.success)

end
