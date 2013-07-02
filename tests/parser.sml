structure ParserTests =
struct
open QCheck
structure E = MonoAST.Expr
structure T = MonoAST.Type
structure D = MonoAST.Decl
structure P = AST.Pattern.Complex
fun test _ =
    let
       fun c name = check (List.getItem, SOME (Show.pair (fn x => x, D.show)))
                          (name, pred (fn (s, ast) => (MonoAST.Decl.make (Parser.parse (Lexer.lexStr s))) = ast))
    in
       (
         c "parser/exprs"
           [
             ("val x1 = 0",               D.Val ("x1", E.Num 0))
            ,("val x2 = foo",             D.Val ("x2", E.Id "foo"))
            ,("val x3 = 1 + 2",           D.Val ("x3", E.Infix (AST.Expr.Add, E.Num 1, E.Num 2)))
            ,("val x4 = 1 * 2 + 3",       D.Val ("x4", E.Infix (AST.Expr.Add, E.Infix (AST.Expr.Mul, E.Num 1, E.Num 2), E.Num 3)))
            ,("val x5 = 1 - 2 / 3",       D.Val ("x5", E.Infix (AST.Expr.Sub, E.Num 1, E.Infix (AST.Expr.Div, E.Num 2, E.Num 3))))
            ,("val x6 = (1 - 2) * 3",     D.Val ("x6", E.Infix (AST.Expr.Mul, E.Infix (AST.Expr.Sub, E.Num 1, E.Num 2), E.Num 3)))
            ,("val x7 = (1 - 2) * (3)",   D.Val ("x7", E.Infix (AST.Expr.Mul, E.Infix (AST.Expr.Sub, E.Num 1, E.Num 2), E.Num 3)))
            ,("val x8 = (bar - 2) / foo", D.Val ("x8", E.Infix (AST.Expr.Div, E.Infix (AST.Expr.Sub, E.Id "bar", E.Num 2), E.Id "foo")))
            ,("val x9 = 1 - 2 + 3 - 4",   D.Val ("x9", E.Infix (AST.Expr.Sub, E.Infix (AST.Expr.Add, E.Infix (AST.Expr.Sub, E.Num 1, E.Num 2), E.Num 3), E.Num 4)))
           ]
       ; c "parser/fns"
           [
             ("val f0 = fn x=>x",           D.Val ("f0", E.Fn ("x", E.Id "x")))
            ,("val f1 = fn x => fn y => y", D.Val ("f1", E.Fn ("x", E.Fn ("y", E.Id "y"))))
            ,("val f2=fn x => x + x",       D.Val ("f2", E.Fn ("x", E.Infix (AST.Expr.Add, E.Id "x", E.Id "x"))))
            ,("val f3=fn x=>x+x",           D.Val ("f3", E.Fn ("x", E.Infix (AST.Expr.Add, E.Id "x", E.Id "x"))))
           ]
       ; c "parser/parens"
           [
             ("val p0 = (1)",                          D.Val ("p0", E.Num 1))
            ,("val p1 = (x)",                          D.Val ("p1", E.Id "x"))
            ,("val p2 =(true)",                        D.Val ("p2", E.Bool true))
            ,("val p3 =if (true) then (x) else ((y))", D.Val ("p3", E.If (E.Bool true, E.Id "x", E.Id "y")))
           ]
       ; c "parser/tuples"
           [
             ("val t0 = (1, 2)", D.Val ("t0", E.Tuple [E.Num 1, E.Num 2]))
            ,("val t1 = (1, 2, 3)", D.Val ("t1", E.Tuple [E.Num 1, E.Num 2, E.Num 3]))
            ,("val t2 = (true, 2, fn x => x)", D.Val ("t2", E.Tuple [E.Bool true, E.Num 2, E.Fn ("x", E.Id "x")]))
           ]
       ; c "parser/app"
           [
             ("val a0 = x y",                              D.Val ("a0", E.App (E.Id "x", E.Id "y")))
            ,("val a1 = (x y)",                            D.Val ("a1", E.App (E.Id "x", E.Id "y")))
            ,("val a2 = (fn x => x) 1",                    D.Val ("a2", E.App (E.Fn ("x", E.Id "x"), E.Num 1)))
            ,("val a3 = (fn f => f 1)",                    D.Val ("a3", E.Fn ("f", E.App (E.Id "f", E.Num 1))))
            ,("val a4 = if not true then false else true", D.Val ("a4", E.If (E.App (E.Id "not", E.Bool true), E.Bool false, E.Bool true)))
            ,("val a5 = if true then not false else true", D.Val ("a5", E.If (E.Bool true, E.App (E.Id "not", E.Bool false), E.Bool true)))
            ,("val a6 = if true then false else not true", D.Val ("a6", E.If (E.Bool true, E.Bool false, E.App (E.Id "not", E.Bool true))))
            ,("val a7 = let val f = fn x => x in f 1 end", D.Val ("a7", E.Let ("f", E.Fn ("x", E.Id "x"), E.App (E.Id "f", E.Num 1))))
           ]
       ; c "parser/match"
           [
             ("val m1 = match f x with y => 0 | z => 1",           D.Val ("m1", E.Match (E.App (E.Id "f", E.Id "x"), [(P.Var "y", E.Num 0), (P.Var "z", E.Num 1)])))
            ,("val m3 = match x with y => if y then 1 else 2",     D.Val ("m3", E.Match (E.Id "x", [(P.Var "y", E.If (E.Id "y", E.Num 1, E.Num 2))])))
            ,("val m4 = match (x) with y => (if y then 1 else 2)", D.Val ("m4", E.Match (E.Id "x", [(P.Var "y", E.If (E.Id "y", E.Num 1, E.Num 2))])))

            ,("val m0 = match x with (Nil) => 0 | (Cons y ys) => 1",
              D.Val ("m0", E.Match (E.Id "x", [(P.Ctor ("Nil", []), E.Num 0), (P.Ctor ("Cons", [P.Var "y", P.Var "ys"]), E.Num 1)])))

            ,("val m2 = match f x with y => g y | z => h z",
              D.Val ("m2", E.Match (E.App (E.Id "f", E.Id "x"), [(P.Var "y", E.App (E.Id "g", E.Id "y")), (P.Var "z", E.App (E.Id "h", E.Id "z"))])))

            ,("val m5 = match x with\n   (Nil)          => 0\n | (Cons y (Nil)) => 1\n | (Cons y ys)    => 2\n",
              D.Val ("m5", E.Match (E.Id "x", [
                         (P.Ctor ("Nil", []), E.Num 0)
                        ,(P.Ctor ("Cons", [P.Var "y", P.Ctor ("Nil", [])]), E.Num 1)
                        ,(P.Ctor ("Cons", [P.Var "y", P.Var "ys"]), E.Num 2)])))
           ]

       ; c "parser/val"
           [
             ("val x = 1",            D.Val ("x", E.Num 1))
            ,("val xx = 1 + 2 + 3",   D.Val ("xx", E.Infix (AST.Expr.Add, E.Infix (AST.Expr.Add, E.Num 1, E.Num 2), E.Num 3)))
            ,("val y = 1 + 2 * 3",    D.Val ("y", E.Infix (AST.Expr.Add, E.Num 1, E.Infix (AST.Expr.Mul, E.Num 2, E.Num 3))))
            ,("val yy = 1 * 2 + 3",   D.Val ("yy", E.Infix (AST.Expr.Add, E.Infix (AST.Expr.Mul, E.Num 1, E.Num 2), E.Num 3)))
            ,("val z = (1 + 2) * 3",  D.Val ("z", E.Infix (AST.Expr.Mul, E.Infix (AST.Expr.Add, E.Num 1, E.Num 2), E.Num 3)))
            ,("val zz = 1 * (2 + 3)", D.Val ("zz", E.Infix (AST.Expr.Mul, E.Num 1, E.Infix (AST.Expr.Add, E.Num 2, E.Num 3))))

            ,("val f = fn x => x",                          D.Val ("f", E.Fn ("x", E.Id "x")))
            ,("val ff = fn x => (x)",                       D.Val ("ff", E.Fn ("x", E.Id "x")))
            ,("val fff = (fn x => x)",                      D.Val ("fff", E.Fn ("x", E.Id "x")))
            ,("val g = fn z => fn y => fn x => x",          D.Val ("g", E.Fn ("z", E.Fn ("y", E.Fn ("x", E.Id "x")))))
            ,("val gg = (fn z => (fn y => (fn x => (x))))", D.Val ("gg", E.Fn ("z", E.Fn ("y", E.Fn ("x", E.Id "x")))))

            ,("val m = let val x = 1 in x end ",            D.Val ("m", E.Let ("x", E.Num 1, E.Id "x")))
            ,("val mm = let val id = fn x => x in id end ", D.Val ("mm", E.Let ("id", E.Fn ("x", E.Id "x"), E.Id "id")))

            ,("val a = if true then false else true",                         D.Val ("a", E.If (E.Bool true, E.Bool false, E.Bool true)))
            ,("val aa = if (true) then (false) else (true)",                  D.Val ("aa", E.If (E.Bool true, E.Bool false, E.Bool true)))
            ,("val b = if true then if false then true else false else true", D.Val ("b", E.If (E.Bool true, E.If (E.Bool false, E.Bool true, E.Bool false), E.Bool true)))

            ,("val bb = if (true) then (if (false) then (true) else (false)) else (true)",
              D.Val ("bb", E.If (E.Bool true, E.If (E.Bool false, E.Bool true, E.Bool false), E.Bool true)))


            ,("val q = match x with\n   (Nil)          => 0\n | (Cons y (Nil)) => 1\n | (Cons y ys)    => 2\n",
              D.Val ("q", E.Match (E.Id "x", [(P.Ctor ("Nil", []), E.Num 0)
                                             ,(P.Ctor ("Cons", [P.Var "y", P.Ctor ("Nil", [])]), E.Num 1)
                                             ,(P.Ctor ("Cons", [P.Var "y", P.Var "ys"]), E.Num 2)])))
            ,("val qq = match x with y => if y then 1 else 2", D.Val ("qq", E.Match (E.Id "x", [(P.Var "y", E.If (E.Id "y", E.Num 1, E.Num 2))])))

            ,("val u = x y", D.Val ("u", E.App (E.Id "x", E.Id "y")))
            ,("val uu = (x y)", D.Val ("uu", E.App (E.Id "x", E.Id "y")))
            ,("val v = let val f = fn x => x in f end 1", D.Val ("v", E.App (E.Let ("f", E.Fn ("x", E.Id "x"), E.Id "f"), E.Num 1)))
            ,("val vv = let val f = fn x => x in f end let val x = 2 in x end", D.Val ("vv", E.App (E.Let ("f", E.Fn ("x", E.Id "x"), E.Id "f"),
                                                                                                   E.Let ("x", E.Num 2, E.Id "x"))))
           ]

       ; c "parser/data"
           [
             ("datatype color = Red | Blue | Green",
              D.Data ([], "color", [("Red", NONE), ("Blue", NONE), ("Green", NONE)]))

            ,("datatype foo = Bar",
              D.Data ([], "foo", [("Bar", NONE)]))

            ,("datatype 'a list1 = Cons of 'a * 'a list1 | Nil",
              D.Data (["a"], "list1", [("Cons", SOME (T.Tuple [T.Var "a", T.Con ("list1", T.Var "a")])), ("Nil", NONE)]))

            ,("datatype 'a list2 = Nil | Cons of 'a * 'a list2",
              D.Data (["a"], "list2", [("Nil", NONE), ("Cons", SOME (T.Tuple [T.Var "a", T.Con ("list2", T.Var "a")]))]))

            ,("datatype 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree",
              D.Data (["a"], "tree", [("Leaf", SOME (T.Var "a")), ("Branch", SOME (T.Tuple [T.Con ("tree", T.Var "a"), T.Con ("tree", T.Var "a")]))]))

            ,("datatype 'a option = None | Some of 'a",
              D.Data (["a"], "option", [("None", NONE), ("Some", SOME (T.Var "a"))]))

            ,("datatype ('a, 'b) either = Left of 'a | Right of 'b",
              D.Data (["a", "b"], "either", [("Left", SOME (T.Var "a")), ("Right", SOME (T.Var "b"))]))
           ]
       )
    end

end
