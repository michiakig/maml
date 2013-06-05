structure TypeInfTests =
struct
local
open TypeInf

   fun showTyp TNum     = "TNum"
     | showTyp TBool    = "TBool"
     | showTyp (TVar s) = "TVar " ^ s

   val typ = {show = showTyp}

   val typeof = Test.group ("typeof", Test.polyAssertEq typ,
                            [
                              {expected = TNum, actual = typeof (Num 0)}
                             ,{expected = TNum, actual = typeof (Succ (Num 0))}
                             ,{expected = TNum, actual = typeof (Pred (Num 0))}
                             ,{expected = TBool, actual = typeof (IsZero (Num 0))}
                             ,{expected = TBool,
                               actual = typeof (IsZero (Pred (Num 0)))}
                           ])

in
   fun main _ = (Test.runTestSuite (true, typeof); OS.Process.success)
end
end
