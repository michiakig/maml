structure TypeInfTests =
struct

structure T = TypeInf
open Syntax

val typ = {show = T.showTyp}

local
   val t = T.typeof o makeAst
in
val typeof = Test.group ("typeof", Test.polyAssertEq typ,
[
  {expected = T.TNum, actual = t (Num 0)}
 ,{expected = T.TNum, actual = t (Succ (Num 0))}
 ,{expected = T.TNum, actual = t (Pred (Num 0))}
 ,{expected = T.TBool, actual = t (IsZero (Num 0))}
 ,{expected = T.TBool, actual = t (IsZero (Pred (Num 0)))}
 ,{expected = T.TNum, actual = t (If (Bool (true), Num 0, Num 1))}
 ,{expected = T.TBool, actual = t (If (IsZero (Num 0), Bool (true), Bool (false)))}
])
end
fun main _ = (
Test.runTestSuite (true, typeof);
OS.Process.success)

end
