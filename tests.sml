structure TypeInfTests =
struct

structure T = TypeInf
open Syntax

val typ = {show = T.showTyp}
val ast = {show = T.showAst}

local
   fun fst (a, _) = a
   val t = T.typeof o (makeAst Syntax.Env.empty)
in
val typeof = Test.group ("typeof", Test.polyAssertEq typ,
[
  {expected = T.TNum, actual = t (Num 0)}
 ,{expected = T.TNum, actual = t (Succ (Num 0))}
 ,{expected = T.TNum, actual = t (Pred (Num 0))}
 ,{expected = T.TBool, actual = t (IsZero (Num 0))}
 ,{expected = T.TBool, actual = t (IsZero (Pred (Num 0)))}
 ,{expected = T.TNum, actual = t (If (Bool (true), Num 0, Num 1))}
 ,{expected = T.TBool, actual = t (If (IsZero (Num 0), Bool true, Bool false))}
 ,{expected = T.TBool, actual = t (If (IsZero (Num 0), Bool true, Bool true))}
 ,{expected = T.TNum, actual = t (If (IsZero (Num 0), Num 0, Num 0))}

 ,{expected = T.TArrow (T.TNum, T.TBool), actual = t (Fun ("x", IsZero (Id "x")))}
 ,{expected = T.TArrow (T.TBool, T.TNum),
   actual = t (Fun ("x", If (Id "x", Num 0, Num 1)))}

 ,{expected = T.TNum, actual = t (App (Fun ("x", Id "x"), Num 0))}
 ,{expected = T.TBool, actual = t (App (Fun ("x", IsZero (Id "x")), Num 0))}

 ,{expected = T.TArrow (T.TVar "a", T.TArrow (T.TVar "b", T.TVar "b")),
   actual = t (Fun ("x", Fun ("y", Id "y")))}
])

(* mostly testing to make sure bound vars get the correct ids *)
val m = fn e => (reset (); makeAst Env.empty e)
val makeAst = (Test.group ("makeAst", Test.polyAssertEq ast,
[
  {expected = T.Num (0, 1), actual = m (Num 1)}
 ,{expected = T.Fun (0, 1, "x", T.Id (0, "x")), actual = m (Fun ("x", Id "x"))}
 ,{expected = T.Fun (0, 5, "x", T.If (1, T.Id (0, "x"), T.Fun (2, 3, "x", T.Id (2, "x")), T.Num (4, 0))),
   actual = m (Fun ("x", If (Id "x", Fun ("x", Id "x"), Num 0)))}
]))

end

fun main _ = (
Test.runTestSuite (true, Test.concat [typeof, makeAst]);
OS.Process.success)

end
