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
val typeof = Test.group ("typeof", Test.polyEq typ,
[
  fn _ => {expected = T.Num, actual = t (S.Num 0)}
 ,fn _ => {expected = T.Num, actual = t (S.Succ (S.Num 0))}
 ,fn _ => {expected = T.Num, actual = t (S.Pred (S.Num 0))}
 ,fn _ => {expected = T.Bool, actual = t (S.IsZero (S.Num 0))}
 ,fn _ => {expected = T.Bool, actual = t (S.IsZero (S.Pred (S.Num 0)))}
 ,fn _ => {expected = T.Num, actual = t (S.If (S.Bool true, S.Num 0, S.Num 1))}
 ,fn _ => {expected = T.Bool, actual = t (S.If (S.IsZero (S.Num 0), S.Bool true, S.Bool false))}
 ,fn _ => {expected = T.Bool, actual = t (S.If (S.IsZero (S.Num 0), S.Bool true, S.Bool true))}
 ,fn _ => {expected = T.Num, actual = t (S.If (S.IsZero (S.Num 0), S.Num 0, S.Num 0))}

 ,fn _ => {expected = T.Arrow (T.Num, T.Bool), actual = t (S.Fun ("x", S.IsZero (S.Id "x")))}
 ,fn _ => {expected = T.Arrow (T.Bool, T.Num),
   actual = t (S.Fun ("x", S.If (S.Id "x", S.Num 0, S.Num 1)))}

 ,fn _ => {expected = T.Num, actual = t (S.App (S.Fun ("x", S.Id "x"), S.Num 0))}
 ,fn _ => {expected = T.Bool, actual = t (S.App (S.Fun ("x", S.IsZero (S.Id "x")), S.Num 0))}

 ,fn _ => {expected = T.Arrow (T.Var "a", T.Arrow (T.Var "b", T.Var "b")),
   actual = t (S.Fun ("x", S.Fun ("y", S.Id "y")))}

 ,fn _ => {expected = T.List T.Num, actual = t (S.Cons (S.Num 0, S.Nil))}
 ,fn _ => {expected = T.List T.Num, actual = t (S.Tl (S.Cons (S.Num 0, S.Nil)))}
 ,fn _ => {expected = T.Num, actual = t (S.Hd (S.Cons (S.Num 0, S.Nil)))}
 ,fn _ => {expected = T.Bool, actual = t (S.IsNil (S.Cons (S.Num 0, S.Nil)))}
 ,fn _ => {expected = T.Arrow (T.Var "a", T.List (T.Var "a")),
   actual = t (S.Fun ("x", S.Cons (S.Id "x", S.Nil)))}

 (* map : (a -> b) -> [a] -> [b] *)
 ,fn _ => {expected = T.Arrow (T.Arrow (T.Var "a", T.Var "b"), T.Arrow (T.List (T.Var "a"), T.List (T.Var "b"))),
   actual= t (S.Rec ("map",
                     S.Fun ("f",
                            S.Fun ("l",
                                   S.If (S.IsNil (S.Id "l"),
                                         S.Nil,
                                         (S.Cons (S.App (S.Id "f", (S.Hd (S.Id "l"))),
                                                  S.apply (S.Id "map", [S.Id "f", S.Tl (S.Id "l")]))))))))}

 (* reduce : (a -> b -> b) -> b -> [a] -> b *)
 ,fn _ => {expected = T.Arrow (T.Arrow (T.Var "a", T.Arrow (T.Var "b", T.Var "b")), T.Arrow (T.Var "b", T.Arrow (T.List (T.Var "a"), T.Var "b"))),
   actual = t (S.Rec ("reduce",
                      S.Fun ("f",
                             S.Fun ("acc",
                                    S.Fun ("l",
                                           S.If (S.IsNil (S.Id "l"),
                                                 S.Id "acc",
                                                 S.apply (S.Id "reduce", [S.Id "f", S.apply (S.Id "f", [S.Hd (S.Id "l"), S.Id "acc"]), S.Tl (S.Id "l")])))))))}
 (* filter : (a -> bool) -> [a] -> [a] *)
 ,fn _ => {expected = T.Arrow (T.Arrow (T.Var "a", T.Bool), (T.Arrow (T.List (T.Var "a"), T.List (T.Var "a")))),
   actual = t (S.Rec ("filter",
                      S.Fun ("f",
                             S.Fun ("l",
                                    S.If (S.App (S.Id "f", S.Hd (S.Id "l")),
                                          S.apply (S.Id "filter", [S.Id "f", S.Tl (S.Id "l")]),
                                          S.Cons (S.Hd (S.Id "l"), S.apply (S.Id "filter", [S.Id "f", S.Tl (S.Id "l")])))))))}
])

(* mostly testing to make sure bound vars get the correct ids *)
val m = fn e => (A.reset (); A.makeAst e)
val makeAst = (Test.group ("makeAst", Test.polyEq ast,
[
  fn _ => {expected = A.Num (0, 1), actual = m (S.Num 1)}
 ,fn _ => {expected = A.Fun (0, 1, "x", A.Id (0, "x")), actual = m (S.Fun ("x", S.Id "x"))}
 ,fn _ => {expected = A.Fun (0, 5, "x", A.If (1, A.Id (0, "x"), A.Fun (2, 3, "x", A.Id (2, "x")), A.Num (4, 0))),
   actual = m (S.Fun ("x", S.If (S.Id "x", S.Fun ("x", S.Id "x"), S.Num 0)))}
]))

val oc = TypeInf.occurs
val occurs = (Test.group ("occurs", Test.polyEq {show = Show.bool},
[
  fn _ =>{expected = false, actual = oc {lhs = T.Var "x", rhs = T.Var "x"}}
,fn _ => {expected = true, actual = oc {lhs = T.Var "x", rhs = T.List (T.Var "x")}}
,fn _ => {expected = true, actual = oc {lhs = T.List (T.Var "x"), rhs = T.Var "x"}}
,fn _ => {expected = true, actual = oc {lhs = T.Var "x", rhs = T.Arrow (T.Num, T.Var "x")}}
]))

val findById = (Test.group ("findById", Test.polyEq {show = Show.option A.show},
[
  fn _ =>{expected = SOME (A.Num (0, 1)), actual = A.findById (A.Num (0, 1), 0)}
 ,fn _ => {expected = NONE,                actual = A.findById (A.Num (0, 1), 1)}
 ,fn _ => {expected = SOME (A.Num (0, 1)), actual = A.findById (A.Pred (1, A.Num (0, 1)), 0)}
 ,fn _ => {expected = SOME (A.Num (0, 1)), actual = A.findById (A.Succ (1, A.Num (0, 1)), 0)}
 ,fn _ => {expected = SOME (A.Num (0, 1)), actual = A.findById (A.IsZero (1, A.Num (0, 1)), 0)}
]))

end

fun main _ = (
Test.runTestSuite (true, Test.concat [typeof, makeAst, occurs, findById]);
OS.Process.success)

end
