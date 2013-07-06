structure ParserTests =
struct
open QCheck
structure E = MonoAST.Expr
structure T = MonoAST.Type
structure D = MonoAST.Decl
structure P = AST.Pattern.Complex
fun test _ =
    let
       fun decl name = check (List.getItem, SOME (Show.pair (fn x => x, D.show)))
                             (name, pred (fn (s, ast) => (MonoAST.Decl.make (hd (Parser.parse (Lexer.lexStr s)))) = ast))

       fun expr name = check (List.getItem, SOME (Show.pair (fn x => x, E.show)))
                             (name, pred (fn (s, ast) => (MonoAST.Expr.make (Parser.parseExpr (Lexer.lexStr s))) = ast))

       fun typ name = check (List.getItem, SOME (Show.pair (fn x => x, T.show)))
                            (name, pred (fn (s, ast) => (MonoAST.Type.make (Parser.parseType (Lexer.lexStr s))) = ast))
    in
       (
         expr "parser/exprs"
           [
             ("0",               E.Num 0)
            ,("foo",             E.Id "foo")
            ,("1 + 2",           E.Infix (AST.Expr.Add, E.Num 1, E.Num 2))
            ,("1 * 2 + 3",       E.Infix (AST.Expr.Add, E.Infix (AST.Expr.Mul, E.Num 1, E.Num 2), E.Num 3))
            ,("1 - 2 / 3",       E.Infix (AST.Expr.Sub, E.Num 1, E.Infix (AST.Expr.Div, E.Num 2, E.Num 3)))
            ,("(1 - 2) * 3",     E.Infix (AST.Expr.Mul, E.Infix (AST.Expr.Sub, E.Num 1, E.Num 2), E.Num 3))
            ,("(1 - 2) * (3)",   E.Infix (AST.Expr.Mul, E.Infix (AST.Expr.Sub, E.Num 1, E.Num 2), E.Num 3))
            ,("(bar - 2) / foo", E.Infix (AST.Expr.Div, E.Infix (AST.Expr.Sub, E.Id "bar", E.Num 2), E.Id "foo"))
            ,("1 - 2 + 3 - 4",   E.Infix (AST.Expr.Sub, E.Infix (AST.Expr.Add, E.Infix (AST.Expr.Sub, E.Num 1, E.Num 2), E.Num 3), E.Num 4))
           ]
       ; expr "parser/fns"
           [
             ("fn x=>x",           E.Fn ("x", E.Id "x"))
            ,("fn x => fn y => y", E.Fn ("x", E.Fn ("y", E.Id "y")))
            ,("fn x => x + x",     E.Fn ("x", E.Infix (AST.Expr.Add, E.Id "x", E.Id "x")))
            ,("fn x=>x+x",         E.Fn ("x", E.Infix (AST.Expr.Add, E.Id "x", E.Id "x")))
           ]
       ; expr "parser/parens"
           [
             ("(1)",                          E.Num 1)
            ,("(x)",                          E.Id "x")
            ,("(true)",                       E.Bool true)
            ,("if (true) then (x) else ((y))",E.If (E.Bool true, E.Id "x", E.Id "y"))
           ]
       ; expr "parser/tuples"
           [
             ("(1, 2)", E.Tuple [E.Num 1, E.Num 2])
            ,("(1, 2, 3)", E.Tuple [E.Num 1, E.Num 2, E.Num 3])
            ,("(true, 2, fn x => x)", E.Tuple [E.Bool true, E.Num 2, E.Fn ("x", E.Id "x")])
           ]
       ; expr "parser/app"
           [
             ("x y",                              E.App (E.Id "x", E.Id "y"))
            ,("(x y)",                            E.App (E.Id "x", E.Id "y"))
            ,("(fn x => x) 1",                    E.App (E.Fn ("x", E.Id "x"), E.Num 1))
            ,("(fn f => f 1)",                    E.Fn ("f", E.App (E.Id "f", E.Num 1)))
            ,("if not true then false else true", E.If (E.App (E.Id "not", E.Bool true), E.Bool false, E.Bool true))
            ,("if true then not false else true", E.If (E.Bool true, E.App (E.Id "not", E.Bool false), E.Bool true))
            ,("if true then false else not true", E.If (E.Bool true, E.Bool false, E.App (E.Id "not", E.Bool true)))
            ,("let val f = fn x => x in f 1 end", E.Let ("f", E.Fn ("x", E.Id "x"), E.App (E.Id "f", E.Num 1)))

            (* function application has higher prec than infix arith operators *)
            ,("f x + g y",       E.Infix (AST.Expr.Add, E.App (E.Id "f", E.Id "x"), E.App (E.Id "g", E.Id "y")))
            ,("f x + g y * h z", E.Infix (AST.Expr.Add, E.App (E.Id "f", E.Id "x"), E.Infix (AST.Expr.Mul, E.App (E.Id "g", E.Id "y"), E.App (E.Id "h", E.Id "z"))))
           ]
       ; expr "parser/case"
           [
             ("case f x of y => 0 | z => 1",           E.Case (E.App (E.Id "f", E.Id "x"), [(P.Var "y", E.Num 0), (P.Var "z", E.Num 1)]))
            ,("case x of y => if y then 1 else 2",     E.Case (E.Id "x", [(P.Var "y", E.If (E.Id "y", E.Num 1, E.Num 2))]))
            ,("case (x) of y => (if y then 1 else 2)", E.Case (E.Id "x", [(P.Var "y", E.If (E.Id "y", E.Num 1, E.Num 2))]))

            ,("case x of Nil => 0 | Cons (y, ys) => 1",
              E.Case (E.Id "x", [(P.Ctor ("Nil", NONE), E.Num 0), (P.Ctor ("Cons", SOME (P.Tuple [P.Var "y", P.Var "ys"])), E.Num 1)]))

            ,("case x of (Nil) => 0 | Cons (y, ys) => 1",
              E.Case (E.Id "x", [(P.Ctor ("Nil", NONE), E.Num 0), (P.Ctor ("Cons", SOME (P.Tuple [P.Var "y", P.Var "ys"])), E.Num 1)]))

            ,("case f x of y => g y | z => h z",
              E.Case (E.App (E.Id "f", E.Id "x"), [(P.Var "y", E.App (E.Id "g", E.Id "y")), (P.Var "z", E.App (E.Id "h", E.Id "z"))]))

            ,("case x of\n   Nil          => 0\n | Cons (y, Nil) => 1\n | Cons (y, ys)    => 2\n",
              E.Case (E.Id "x", [
                         (P.Ctor ("Nil", NONE), E.Num 0)
                        ,(P.Ctor ("Cons", SOME (P.Tuple [P.Var "y", P.Ctor ("Nil", NONE)])), E.Num 1)
                        ,(P.Ctor ("Cons", SOME (P.Tuple [P.Var "y", P.Var "ys"])), E.Num 2)]))
           ]

       ; typ "parser/type"
             [
               ("'a", T.Var "a")
              ,("'a list tree", T.Con ("tree", T.Con ("list", T.Var "a")))
              ,("'a list", T.Con ("list", T.Var "a"))
              (* ,("('a, 'b) either", ... ) *)

              ,("'a * 'b", T.Tuple [T.Var "a", T.Var "b"])
              ,("'a * 'b * 'c", T.Tuple [T.Var "a", T.Var "b", T.Var "c"])
              ,("('a)", T.Paren (T.Var "a"))
              ,("('a * 'b) * 'c", T.Tuple [T.Paren (T.Tuple [T.Var "a", T.Var "b"]), T.Var "c"])
              ,("'a * ('b * 'c)", T.Tuple [T.Var "a", T.Paren (T.Tuple [T.Var "b", T.Var "c"])])

              (* ctor app has higher prec than * (tuple op) *)
              ,("'a list * 'b", T.Tuple [T.Con ("list", T.Var "a"), T.Var "b"])
              ,("'a * 'b list", T.Tuple [T.Var "a", T.Con ("list", T.Var "b")])
              ,("('a * 'b) list", T.Con ("list", T.Paren (T.Tuple [T.Var "a", T.Var "b"])))

              (* ... and higher prec than -> (arrow) *)
              ,("'a list -> 'a", T.Arrow (T.Con ("list", T.Var "a"), T.Var "a"))
              ,("'a -> 'a list", T.Arrow (T.Var "a", T.Con ("list", T.Var "a")))
              ,("('a -> 'a) list", T.Con ("list", T.Paren (T.Arrow (T.Var "a", T.Var "a"))))

              (* arrow associates to the right *)
              ,("'a -> 'a", T.Arrow (T.Var "a", T.Var "a"))
              ,("'a -> 'a -> 'a", T.Arrow (T.Var "a", T.Arrow (T.Var "a", T.Var "a")))
              ,("('a -> 'a) -> 'a", T.Arrow (T.Paren (T.Arrow (T.Var "a", T.Var "a")), T.Var "a"))
              ,("('a -> 'a) -> 'a -> 'a", T.Arrow (T.Paren (T.Arrow (T.Var "a", T.Var "a")), T.Arrow (T.Var "a", T.Var "a")))

              ,("'a * 'b -> 'c", T.Arrow (T.Tuple [T.Var "a", T.Var "b"], T.Var "c"))
              ,("('a * 'b) -> 'c", T.Arrow (T.Paren (T.Tuple [T.Var "a", T.Var "b"]), T.Var "c"))
              ,("'a * ('b -> 'c)", T.Tuple [T.Var "a", T.Paren (T.Arrow (T.Var "b", T.Var "c"))])
             ]

       ; decl "parser/val"
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


            ,("val q = case x of\n   Nil          => 0\n | Cons (y, Nil) => 1\n | Cons (y, ys)    => 2\n",
              D.Val ("q", E.Case (E.Id "x", [(P.Ctor ("Nil", NONE), E.Num 0)
                                             ,(P.Ctor ("Cons", SOME (P.Tuple [P.Var "y", P.Ctor ("Nil", NONE)])), E.Num 1)
                                             ,(P.Ctor ("Cons", SOME (P.Tuple [P.Var "y", P.Var "ys"])), E.Num 2)])))
            ,("val qq = case x of y => if y then 1 else 2", D.Val ("qq", E.Case (E.Id "x", [(P.Var "y", E.If (E.Id "y", E.Num 1, E.Num 2))])))

            ,("val u = x y", D.Val ("u", E.App (E.Id "x", E.Id "y")))
            ,("val uu = (x y)", D.Val ("uu", E.App (E.Id "x", E.Id "y")))
            ,("val v = let val f = fn x => x in f end 1", D.Val ("v", E.App (E.Let ("f", E.Fn ("x", E.Id "x"), E.Id "f"), E.Num 1)))
            ,("val vv = let val f = fn x => x in f end let val x = 2 in x end", D.Val ("vv", E.App (E.Let ("f", E.Fn ("x", E.Id "x"), E.Id "f"),
                                                                                                   E.Let ("x", E.Num 2, E.Id "x"))))
           ]

       ; decl "parser/data"
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
