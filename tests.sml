structure TypeInfTests =
struct

open TypeInf

   val typ = {show = showTyp}

   val typeof = Test.group ("typeof", Test.polyAssertEq typ,
       [
         {expected = TNum, actual = typeof (Num 0)}
        ,{expected = TNum, actual = typeof (Succ (Num 0))}
        ,{expected = TNum, actual = typeof (Pred (Num 0))}
        ,{expected = TBool, actual = typeof (IsZero (Num 0))}
        ,{expected = TBool,
          actual = typeof (IsZero (Pred (Num 0)))}

        ,{expected = TNum,
          actual = typeof (If (Bool true, Num 0, Num 1))}
        ,{expected = TBool,
          actual = typeof (If (IsZero (Num 0), Bool true, Bool false))}

      ])

   fun main _ = (Test.runTestSuite (true, typeof); OS.Process.success)

end
