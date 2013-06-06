structure TypeInfTests =
struct
local
open TypeInf

   fun showTyp TNum     = "TNum"
     | showTyp TBool    = "TBool"
     | showTyp (TVar s) = "TVar " ^ s
     | showTyp (TArrow (t1, t2)) =
       "TArrow (" ^ showTyp t1 ^ "," ^ showTyp t2 ^ "," ^ ")"

   fun showAst (Bool b)   = "Bool " ^ Bool.toString b
     | showAst (Num n)    = "Num " ^ Int.toString n
     | showAst (Succ e)   = "Succ (" ^ showAst e  ^ ")"
     | showAst (Pred e)   = "Pred (" ^ showAst e  ^ ")"
     | showAst (IsZero e) = "IsZero (" ^ showAst e  ^ ")"
     | showAst (If (e1, e2, e3)) =
       "If (" ^ showAst e1 ^ "," ^ showAst e2 ^ "," ^ showAst e3 ^ ")"
     | showAst (App (e1, e2)) = "App (" ^ showAst e1 ^ "," ^ showAst e2 ^ ")"
     | showAst (Fun (x, e)) = "Fun (" ^ x ^ "," ^ showAst e ^ ")"
     | showAst (Id x) = "Id " ^ x

   structure ShowAstStringMap =
      MapShowFn(structure Map = StringMap
                structure K = struct
                   type t = string
                   val show = Show.string
                end
                structure V = struct
                   type t = ast
                   val show = showAst
                end)

   fun showConstr ({lhs, rhs} : constr) = "{" ^ lhs ^ "," ^ showTyp rhs ^ "}"

   structure ShowConstraintSet =
      SetShowFn(structure Set = ConstrSet
                structure Show = struct
                   type t = constr
                   val show = showConstr
                end)

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
