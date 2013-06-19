structure TypeInfTests =
struct

open QCheck infix ==>

structure T = Type
structure S = Syntax.Concrete
structure A = Syntax.Abstract

fun main _ = (
   check (List.getItem, SOME (Show.pair (T.show, S.show)))
         ("typeof", pred (fn (ty, syn) => (TypeInf.typeof (A.makeAst syn)) = ty))
         [
           (T.Num, S.Num 0)
          ,(T.Num, S.Succ (S.Num 0))
          ,(T.Num, S.Pred (S.Num 0))
          ,(T.Bool, S.IsZero (S.Num 0))
          ,(T.Bool, S.IsZero (S.Pred (S.Num 0)))
          ,(T.Num, S.If (S.Bool true, S.Num 0, S.Num 1))
          ,(T.Bool, S.If (S.IsZero (S.Num 0), S.Bool true, S.Bool false))
          ,(T.Bool, S.If (S.IsZero (S.Num 0), S.Bool true, S.Bool true))
          ,(T.Num, S.If (S.IsZero (S.Num 0), S.Num 0, S.Num 0))

          ,(T.Arrow (T.Num, T.Bool), S.Fun ("x", S.IsZero (S.Id "x")))
          ,(T.Arrow (T.Bool, T.Num), S.Fun ("x", S.If (S.Id "x", S.Num 0, S.Num 1)))

          ,(T.Num, S.App (S.Fun ("x", S.Id "x"), S.Num 0))
          ,(T.Bool, S.App (S.Fun ("x", S.IsZero (S.Id "x")), S.Num 0))

          ,(T.Arrow (T.Var "a", T.Arrow (T.Var "b", T.Var "b")), S.Fun ("x", S.Fun ("y", S.Id "y")))

          ,(T.List T.Num, S.Cons (S.Num 0, S.Nil))
          ,(T.List T.Num, S.Tl (S.Cons (S.Num 0, S.Nil)))
          ,(T.Num, S.Hd (S.Cons (S.Num 0, S.Nil)))
          ,(T.Bool, S.IsNil (S.Cons (S.Num 0, S.Nil)))
          ,(T.Arrow (T.Var "a", T.List (T.Var "a")), S.Fun ("x", S.Cons (S.Id "x", S.Nil)))

          (* map : (a -> b) -> [a] -> [b] *)
          ,(T.Arrow (T.Arrow (T.Var "a", T.Var "b"), T.Arrow (T.List (T.Var "a"), T.List (T.Var "b"))),
            S.Rec ("map",
                   S.Fun ("f",
                          S.Fun ("l",
                                 S.If (S.IsNil (S.Id "l"),
                                       S.Nil,
                                       (S.Cons (S.App (S.Id "f", (S.Hd (S.Id "l"))),
                                                S.apply (S.Id "map", [S.Id "f", S.Tl (S.Id "l")]))))))))

          (* reduce : (a -> b -> b) -> b -> [a] -> b *)
          ,(T.Arrow (T.Arrow (T.Var "a", T.Arrow (T.Var "b", T.Var "b")), T.Arrow (T.Var "b", T.Arrow (T.List (T.Var "a"), T.Var "b"))),
            S.Rec ("reduce",
                   S.Fun ("f",
                          S.Fun ("acc",
                                 S.Fun ("l",
                                        S.If (S.IsNil (S.Id "l"),
                                              S.Id "acc",
                                              S.apply (S.Id "reduce", [S.Id "f", S.apply (S.Id "f", [S.Hd (S.Id "l"), S.Id "acc"]), S.Tl (S.Id "l")])))))))
          (* filter : (a -> bool) -> [a] -> [a] *)
          ,(T.Arrow (T.Arrow (T.Var "a", T.Bool), (T.Arrow (T.List (T.Var "a"), T.List (T.Var "a")))),
            S.Rec ("filter",
                   S.Fun ("f",
                          S.Fun ("l",
                                 S.If (S.App (S.Id "f", S.Hd (S.Id "l")),
                                       S.apply (S.Id "filter", [S.Id "f", S.Tl (S.Id "l")]),
                                       S.Cons (S.Hd (S.Id "l"), S.apply (S.Id "filter", [S.Id "f", S.Tl (S.Id "l")])))))))
         ]

 (* mostly testing to make sure bound vars get the correct ids *)
 ; check (List.getItem, SOME (Show.pair (A.show, S.show)))
         ("makeAst", pred (fn (ast, syn) => (A.reset (); (A.makeAst syn) = ast)))
         [
           (A.Num (0, 1), S.Num 1)
          ,(A.Fun (0, 1, "x", A.Id (0, "x")), S.Fun ("x", S.Id "x"))
          ,(A.Fun (0, 5, "x", A.If (1, A.Id (0, "x"), A.Fun (2, 3, "x", A.Id (2, "x")), A.Num (4, 0))), S.Fun ("x", S.If (S.Id "x", S.Fun ("x", S.Id "x"), S.Num 0)))
         ]

 ; check (List.getItem, NONE)
         ("occursCheck1", pred TypeInf.occurs)
         [
           {lhs = T.Var "x", rhs = T.List (T.Var "x")}
         , {lhs = T.List (T.Var "x"), rhs = T.Var "x"}
         , {lhs = T.Var "x", rhs = T.Arrow (T.Num, T.Var "x")}
         ]

 ; check (List.getItem, NONE)
         ("occursCheck2", pred (not o TypeInf.occurs))
         [
           {lhs = T.Var "x", rhs = T.Var "x"}
         ]

 ; check (List.getItem, NONE)
         ("findById", pred (fn (res, (ast, id)) => res = A.findById (ast, id)))
         [
           (SOME (A.Num (0, 1)), (A.Num (0, 1), 0))
          ,(NONE,                (A.Num (0, 1), 1))
          ,(SOME (A.Num (0, 1)), (A.Pred (1, A.Num (0, 1)), 0))
          ,(SOME (A.Num (0, 1)), (A.Succ (1, A.Num (0, 1)), 0))
          ,(SOME (A.Num (0, 1)), (A.IsZero (1, A.Num (0, 1)), 0))
         ]

 ; OS.Process.success)

end
