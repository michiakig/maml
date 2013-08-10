structure TypeInfTests =
struct

open QCheck infix ==>

structure T = Type

fun test _ = (
   check (List.getItem, SOME (Show.pair (T.show, fn x => x)))
         ("typeof", pred (fn (ty, s) => Type.normalize (Typecheck.gettyp (Typecheck.inferExpr (Typecheck.initEnv, Parser.parseExpr (Lexer.lexStr s)))) = ty))
         [
           (T.Num, "0")
          (* ,(T.Num, "0 + 1") *)
          (* ,(T.Num, "0 - 1") *)
          ,(T.Num, "if true then 0 else 1")
          ,(T.Bool, "if true then true else false")

          (* ,(T.Arrow (T.Num, T.Bool), S.Fun ("x", S.IsZero (S.Id "x"))) *)
          ,(T.Arrow (T.Bool, T.Num), "fn x => if x then 0 else 1")
          (* ,(T.Arrow (T.Num, T.Num), "fn x => x + x") *)
          ,(T.Arrow (T.Var "a", T.Var "a"), "fn x => x")

          ,(T.Num, "(fn x => x) 0")
          (* ,(T.Bool, S.App (S.Fun ("x", S.IsZero (S.Id "x")), S.Num 0)) *)

          ,(T.Arrow (T.Var "a", T.Arrow (T.Var "b", T.Var "b")), "fn x => fn y => y")
          ,(T.Arrow (T.Var "a", T.Arrow (T.Var "b", T.Var "a")), "fn x => fn y => x")

          (* ,(T.List T.Num, S.Cons (S.Num 0, S.Nil)) *)
          (* ,(T.List T.Num, S.Tl (S.Cons (S.Num 0, S.Nil))) *)
          (* ,(T.Num, S.Hd (S.Cons (S.Num 0, S.Nil))) *)
          (* ,(T.Bool, S.IsNil (S.Cons (S.Num 0, S.Nil))) *)
          (* ,(T.Arrow (T.Var "a", T.List (T.Var "a")), S.Fun ("x", S.Cons (S.Id "x", S.Nil))) *)

          (* (* map : (a -> b) -> [a] -> [b] *) *)
          (* ,(T.Arrow (T.Arrow (T.Var "a", T.Var "b"), T.Arrow (T.List (T.Var "a"), T.List (T.Var "b"))), *)
          (*   S.Rec ("map", *)
          (*          S.Fun ("f", *)
          (*                 S.Fun ("l", *)
          (*                        S.If (S.IsNil (S.Id "l"), *)
          (*                              S.Nil, *)
          (*                              (S.Cons (S.App (S.Id "f", (S.Hd (S.Id "l"))), *)
          (*                                       S.apply (S.Id "map", [S.Id "f", S.Tl (S.Id "l")])))))))) *)

          (* (* reduce : (a -> b -> b) -> b -> [a] -> b *) *)
          (* ,(T.Arrow (T.Arrow (T.Var "a", T.Arrow (T.Var "b", T.Var "b")), T.Arrow (T.Var "b", T.Arrow (T.List (T.Var "a"), T.Var "b"))), *)
          (*   S.Rec ("reduce", *)
          (*          S.Fun ("f", *)
          (*                 S.Fun ("acc", *)
          (*                        S.Fun ("l", *)
          (*                               S.If (S.IsNil (S.Id "l"), *)
          (*                                     S.Id "acc", *)
          (*                                     S.apply (S.Id "reduce", [S.Id "f", S.apply (S.Id "f", [S.Hd (S.Id "l"), S.Id "acc"]), S.Tl (S.Id "l")]))))))) *)
          (* (* filter : (a -> bool) -> [a] -> [a] *) *)
          (* ,(T.Arrow (T.Arrow (T.Var "a", T.Bool), (T.Arrow (T.List (T.Var "a"), T.List (T.Var "a")))), *)
          (*   S.Rec ("filter", *)
          (*          S.Fun ("f", *)
          (*                 S.Fun ("l", *)
          (*                        S.If (S.App (S.Id "f", S.Hd (S.Id "l")), *)
          (*                              S.apply (S.Id "filter", [S.Id "f", S.Tl (S.Id "l")]), *)
          (*                              S.Cons (S.Hd (S.Id "l"), S.apply (S.Id "filter", [S.Id "f", S.Tl (S.Id "l")]))))))) *)
         ])

end
