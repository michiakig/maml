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
       (*   c "parser/exprs" *)
       (*     [ *)
       (*       ("0",               M.Num 0) *)
       (*      ,("foo",             M.Id "foo") *)
       (*      ,("1 + 2",           M.Infix (E.Add, M.Num 1, M.Num 2)) *)
       (*      ,("1 * 2 + 3",       M.Infix (E.Add, M.Infix (E.Mul, M.Num 1, M.Num 2), M.Num 3)) *)
       (*      ,("1 - 2 / 3",       M.Infix (E.Sub, M.Num 1, M.Infix (E.Div, M.Num 2, M.Num 3))) *)
       (*      ,("(1 - 2) * 3",     M.Infix (E.Mul, M.Infix (E.Sub, M.Num 1, M.Num 2), M.Num 3)) *)
       (*      ,("(1 - 2) * (3)",   M.Infix (E.Mul, M.Infix (E.Sub, M.Num 1, M.Num 2), M.Num 3)) *)
       (*      ,("(bar - 2) / foo", M.Infix (E.Div, M.Infix (E.Sub, M.Id "bar", M.Num 2), M.Id "foo")) *)
       (*      ,("1 - 2 + 3 - 4",   M.Infix (E.Sub, M.Infix (E.Add, M.Infix (E.Sub, M.Num 1, M.Num 2), M.Num 3), M.Num 4)) *)
       (*     ] *)
       (* ; c "parser/fns" *)
       (*     [ *)
       (*       ("fn x=>x",           M.Fn ("x", M.Id "x")) *)
       (*      ,("fn x => fn y => y", M.Fn ("x", M.Fn ("y", M.Id "y"))) *)
       (*      ,("fn x => x + x",     M.Fn ("x", M.Infix (E.Add, M.Id "x", M.Id "x"))) *)
       (*      ,("fn x=>x+x",         M.Fn ("x", M.Infix (E.Add, M.Id "x", M.Id "x"))) *)
       (*     ] *)
       (* ; c "parser/parens" *)
       (*     [ *)
       (*       ("(1)",                           M.Num 1) *)
       (*      ,("(x)",                           M.Id "x") *)
       (*      ,("(true)",                        M.Bool true) *)
       (*      ,("if (true) then (x) else ((y))", M.If (M.Bool true, M.Id "x", M.Id "y")) *)
       (*     ] *)
       (* ; c "parser/app" *)
       (*     [ *)
       (*       ("x y",                              M.App (M.Id "x", M.Id "y")) *)
       (*      ,("(x y)",                            M.App (M.Id "x", M.Id "y")) *)
       (*      ,("(fn x => x) 1",                    M.App (M.Fn ("x", M.Id "x"), M.Num 1)) *)
       (*      ,("(fn f => f 1)",                    M.Fn ("f", M.App (M.Id "f", M.Num 1))) *)
       (*      ,("if not true then false else true", M.If (M.App (M.Id "not", M.Bool true), M.Bool false, M.Bool true)) *)
       (*      ,("if true then not false else true", M.If (M.Bool true, M.App (M.Id "not", M.Bool false), M.Bool true)) *)
       (*      ,("if true then false else not true", M.If (M.Bool true, M.Bool false, M.App (M.Id "not", M.Bool true))) *)
       (*      ,("let val f = fn x => x in f 1 end", M.Let ("f", M.Fn ("x", M.Id "x"), M.App (M.Id "f", M.Num 1))) *)
       (*     ] *)
       (* ; c "parser/match" *)
       (*     [ *)
       (*       ("match x with (Nil) => 0 | (Cons y ys) => 1", M.Match (M.Id "x", [(Pat.Ctor ("Nil", []), M.Num 0), (Pat.Ctor ("Cons", [Pat.Var "y", Pat.Var "ys"]), M.Num 1)])) *)
       (*      ,("match f x with y => 0 | z => 1", M.Match (M.App (M.Id "f", M.Id "x"), [(Pat.Var "y", M.Num 0), (Pat.Var "z", M.Num 1)])) *)
       (*      ,("match f x with y => g y | z => h z", M.Match (M.App (M.Id "f", M.Id "x"), [(Pat.Var "y", M.App (M.Id "g", M.Id "y")), (Pat.Var "z", M.App (M.Id "h", M.Id "z"))])) *)
       (*      ,("match x with y => if y then 1 else 2", M.Match (M.Id "x", [(Pat.Var "y", M.If (M.Id "y", M.Num 1, M.Num 2))])) *)
       (*      ,("match (x) with y => (if y then 1 else 2)", M.Match (M.Id "x", [(Pat.Var "y", M.If (M.Id "y", M.Num 1, M.Num 2))])) *)
       (*      ,("match x with\n   (Nil)          => 0\n | (Cons y (Nil)) => 1\n | (Cons y ys)    => 2\n", *)
       (*        M.Match (M.Id "x", [ *)
       (*                   (Pat.Ctor ("Nil", []), M.Num 0) *)
       (*                  ,(Pat.Ctor ("Cons", [Pat.Var "y", Pat.Ctor ("Nil", [])]), M.Num 1) *)
       (*                  ,(Pat.Ctor ("Cons", [Pat.Var "y", Pat.Var "ys"]), M.Num 2) *)
       (*       ])) *)

         c "parser/val"
           [
             ("val x = 1", D.Val ("x", E.Num 1))
            ,("val xx = 1 + 2 + 3", D.Val ("xx", E.Infix (AST.Expr.Add, E.Infix (AST.Expr.Add, E.Num 1, E.Num 2), E.Num 3)))
            ,("val y = 1 + 2 * 3", D.Val ("y", E.Infix (AST.Expr.Add, E.Num 1, E.Infix (AST.Expr.Mul, E.Num 2, E.Num 3))))
            ,("val yy = 1 * 2 + 3", D.Val ("yy", E.Infix (AST.Expr.Add, E.Infix (AST.Expr.Mul, E.Num 1, E.Num 2), E.Num 3)))
            ,("val z = (1 + 2) * 3", D.Val ("z", E.Infix (AST.Expr.Mul, E.Infix (AST.Expr.Add, E.Num 1, E.Num 2), E.Num 3)))
            ,("val zz = 1 * (2 + 3)", D.Val ("zz", E.Infix (AST.Expr.Mul, E.Num 1, E.Infix (AST.Expr.Add, E.Num 2, E.Num 3))))

            ,("val f = fn x => x", D.Val ("f", E.Fn ("x", E.Id "x")))
            ,("val ff = fn x => (x)", D.Val ("ff", E.Fn ("x", E.Id "x")))
            ,("val fff = (fn x => x)", D.Val ("fff", E.Fn ("x", E.Id "x")))
            ,("val g = fn z => fn y => fn x => x", D.Val ("g", E.Fn ("z", E.Fn ("y", E.Fn ("x", E.Id "x")))))
            ,("val gg = (fn z => (fn y => (fn x => (x))))", D.Val ("gg", E.Fn ("z", E.Fn ("y", E.Fn ("x", E.Id "x")))))

            ,("val m = let val x = 1 in x end ", D.Val ("m", E.Let ("x", E.Num 1, E.Id "x")))
            ,("val mm = let val id = fn x => x in id end ", D.Val ("mm", E.Let ("id", E.Fn ("x", E.Id "x"), E.Id "id")))

            ,("val a = if true then false else true", D.Val ("a", E.If (E.Bool true, E.Bool false, E.Bool true)))
            ,("val aa = if (true) then (false) else (true)", D.Val ("aa", E.If (E.Bool true, E.Bool false, E.Bool true)))
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
             ("datatype list = Nil | Cons of 'a * 'a list",
              D.Data ("list", [("Nil", NONE), ("Cons", SOME (T.Tuple [T.Var "a", T.Con ("list", T.Var "a")]))]))

            ,("datatype tree = Leaf of 'a | Branch of 'a tree * 'a tree",
              D.Data ("tree", [("Leaf", SOME (T.Var "a")), ("Branch", SOME (T.Tuple [T.Con ("tree", T.Var "a"), T.Con ("tree", T.Var "a")]))]))

            ,("datatype option = None | Some of 'a",
              D.Data ("option", [("None", NONE), ("Some", SOME (T.Var "a"))]))
           ]
       )
    end

end
