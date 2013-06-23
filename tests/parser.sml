structure ParserTests =
struct
open QCheck
structure A = Abstract
structure Pat = Pattern.Complex
fun test _ =
    let
       fun c name = check (List.getItem, SOME (Show.pair (fn x => x, A.show)))
                          (name, pred (fn (s, ast) => (Parser.parse (Lexer.lexStr s)) = ast))
    in
       (
         c "parser/exprs"
           [
             ("0",               A.Num 0)
            ,("foo",             A.Id "foo")
            ,("1 + 2",           A.Add (A.Num 1, A.Num 2))
            ,("1 * 2 + 3",       A.Add (A.Mul (A.Num 1, A.Num 2), A.Num 3))
            ,("1 - 2 / 3",       A.Sub (A.Num 1, A.Div (A.Num 2, A.Num 3)))
            ,("(1 - 2) * 3",     A.Mul (A.Sub (A.Num 1, A.Num 2), A.Num 3))
            ,("(1 - 2) * (3)",   A.Mul (A.Sub (A.Num 1, A.Num 2), A.Num 3))
            ,("(bar - 2) / foo", A.Div (A.Sub (A.Id "bar", A.Num 2), A.Id "foo"))
            ,("1 - 2 + 3 - 4",   A.Sub (A.Add (A.Sub (A.Num 1, A.Num 2), A.Num 3), A.Num 4))
           ]
       ; c "parser/fns"
           [
             ("fn x=>x",           A.Fn ("x", A.Id "x"))
            ,("fn x => fn y => y", A.Fn ("x", A.Fn ("y", A.Id "y")))
            ,("fn x => x + x",     A.Fn ("x", A.Add (A.Id "x", A.Id "x")))
            ,("fn x=>x+x",         A.Fn ("x", A.Add (A.Id "x", A.Id "x")))
           ]
       ; c "parser/parens"
           [
             ("(1)",                           A.Num 1)
            ,("(x)",                           A.Id "x")
            ,("(true)",                        A.Bool true)
            ,("if (true) then (x) else ((y))", A.If (A.Bool true, A.Id "x", A.Id "y"))
           ]
       ; c "parser/app"
           [
             ("x y",                              A.App (A.Id "x", A.Id "y"))
            ,("(x y)",                            A.App (A.Id "x", A.Id "y"))
            ,("(fn x => x) 1",                    A.App (A.Fn ("x", A.Id "x"), A.Num 1))
            ,("(fn f => f 1)",                    A.Fn ("f", A.App (A.Id "f", A.Num 1)))
            ,("if not true then false else true", A.If (A.App (A.Id "not", A.Bool true), A.Bool false, A.Bool true))
            ,("if true then not false else true", A.If (A.Bool true, A.App (A.Id "not", A.Bool false), A.Bool true))
            ,("if true then false else not true", A.If (A.Bool true, A.Bool false, A.App (A.Id "not", A.Bool true)))
            ,("let f = fn x => x in f 1",         A.Let ("f", A.Fn ("x", A.Id "x"), A.App (A.Id "f", A.Num 1)))
           ]
       ; c "parser/match"
           [
             ("match x with (Nil) => 0 | (Cons y ys) => 1", A.Match (A.Id "x", [(Pat.Ctor ("Nil", []), A.Num 0), (Pat.Ctor ("Cons", [Pat.Var "y", Pat.Var "ys"]), A.Num 1)]))
            ,("match f x with y => 0 | z => 1", A.Match (A.App (A.Id "f", A.Id "x"), [(Pat.Var "y", A.Num 0), (Pat.Var "z", A.Num 1)]))
            ,("match f x with y => g y | z => h z", A.Match (A.App (A.Id "f", A.Id "x"), [(Pat.Var "y", A.App (A.Id "g", A.Id "y")), (Pat.Var "z", A.App (A.Id "h", A.Id "z"))]))
            ,("match x with y => if y then 1 else 2", A.Match (A.Id "x", [(Pat.Var "y", A.If (A.Id "y", A.Num 1, A.Num 2))]))
            ,("match (x) with y => (if y then 1 else 2)", A.Match (A.Id "x", [(Pat.Var "y", A.If (A.Id "y", A.Num 1, A.Num 2))]))
            ,("match x with\n   (Nil)          => 0\n | (Cons y (Nil)) => 1\n | (Cons y ys)    => 2\n",
              A.Match (A.Id "x", [
                         (Pat.Ctor ("Nil", []), A.Num 0)
                        ,(Pat.Ctor ("Cons", [Pat.Var "y", Pat.Ctor ("Nil", [])]), A.Num 1)
                        ,(Pat.Ctor ("Cons", [Pat.Var "y", Pat.Var "ys"]), A.Num 2)
             ]))
           ]
       )
    end

end
