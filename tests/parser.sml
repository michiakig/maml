structure ParserTests =
struct
open QCheck
structure A = Abstract
structure M = Abstract.Mono
structure Pat = Pattern.Complex
fun test _ =
    let
       fun c name = check (List.getItem, SOME (Show.pair (fn x => x, A.show)))
                          (name, pred (fn (s, ast) => (Parser.parse (Lexer.lexStr s)) = ast))
       fun c' name = check (List.getItem, SOME (Show.pair (fn x => x, M.show)))
                           (name, pred (fn (s, ast) => (A.toMono (Parser.parse (Lexer.lexStr s))) = ast))
    in
       (
         c "parser/exprs"
           [
             ("0",               A.Num ({line=1,col=1}, 0))
            ,("foo",             A.Id ({line=1,col=1}, "foo"))
            ,("1 + 2",           A.Add ({line=1,col=3}, A.Num ({line=1,col=1},1), A.Num ({line=1,col=5},2)))
            ,("1 * 2 + 3",       A.Add ({line=1,col=7}, A.Mul ({line=1,col=3}, A.Num ({line=1,col=1},1), A.Num ({line=1,col=5},2)), A.Num ({line=1,col=9},3)))
            ,("1 - 2 / 3",       A.Sub ({line=1,col=3},A.Num ({line=1,col=1},1), A.Div ({line=1,col=7}, A.Num ({line=1,col=5},2), A.Num ({line=1,col=9},3))))
            ,("(1 - 2) * 3",     A.Mul ({line=1,col=9},A.Sub ({line=1,col=4},A.Num ({line=1,col=2},1), A.Num ({line=1,col=6},2)), A.Num ({line=1,col=11},3)))
            ,("(1 - 2) * (3)",   A.Mul ({line=1,col=9},A.Sub ({line=1,col=4}, A.Num ({line=1,col=2},1), A.Num ({line=1,col=6},2)), A.Num ({line=1,col=12},3)))
            ,("(bar - 2) / foo", A.Div ({line=1,col=11},A.Sub ({line=1,col=6}, A.Id ({line=1,col=2},"bar"), A.Num ({line=1,col=8},2)), A.Id ({line=1,col=13},"foo")))
            ,("1 - 2 + 3 - 4",   A.Sub ({line=1,col=11}, A.Add ({line=1,col=7}, A.Sub ({line=1,col=3}, A.Num ({line=1,col=1},1), A.Num ({line=1,col=5},2)), A.Num ({line=1,col=9},3)), A.Num ({line=1,col=13},4)))
           ]
       ; c' "parser/fns"
           [
             ("fn x=>x",           M.Fn ("x", M.Id "x"))
            ,("fn x => fn y => y", M.Fn ("x", M.Fn ("y", M.Id "y")))
            ,("fn x => x + x",     M.Fn ("x", M.Add (M.Id "x", M.Id "x")))
            ,("fn x=>x+x",         M.Fn ("x", M.Add (M.Id "x", M.Id "x")))
           ]
       ; c' "parser/parens"
           [
             ("(1)",                           M.Num 1)
            ,("(x)",                           M.Id "x")
            ,("(true)",                        M.Bool true)
            ,("if (true) then (x) else ((y))", M.If (M.Bool true, M.Id "x", M.Id "y"))
           ]
       ; c' "parser/app"
           [
             ("x y",                              M.App (M.Id "x", M.Id "y"))
            ,("(x y)",                            M.App (M.Id "x", M.Id "y"))
            ,("(fn x => x) 1",                    M.App (M.Fn ("x", M.Id "x"), M.Num 1))
            ,("(fn f => f 1)",                    M.Fn ("f", M.App (M.Id "f", M.Num 1)))
            ,("if not true then false else true", M.If (M.App (M.Id "not", M.Bool true), M.Bool false, M.Bool true))
            ,("if true then not false else true", M.If (M.Bool true, M.App (M.Id "not", M.Bool false), M.Bool true))
            ,("if true then false else not true", M.If (M.Bool true, M.Bool false, M.App (M.Id "not", M.Bool true)))
            ,("let f = fn x => x in f 1",         M.Let ("f", M.Fn ("x", M.Id "x"), M.App (M.Id "f", M.Num 1)))
           ]
       ; c' "parser/match"
           [
             ("match x with (Nil) => 0 | (Cons y ys) => 1", M.Match (M.Id "x", [(Pat.Ctor ("Nil", []), M.Num 0), (Pat.Ctor ("Cons", [Pat.Var "y", Pat.Var "ys"]), M.Num 1)]))
            ,("match f x with y => 0 | z => 1", M.Match (M.App (M.Id "f", M.Id "x"), [(Pat.Var "y", M.Num 0), (Pat.Var "z", M.Num 1)]))
            ,("match f x with y => g y | z => h z", M.Match (M.App (M.Id "f", M.Id "x"), [(Pat.Var "y", M.App (M.Id "g", M.Id "y")), (Pat.Var "z", M.App (M.Id "h", M.Id "z"))]))
            ,("match x with y => if y then 1 else 2", M.Match (M.Id "x", [(Pat.Var "y", M.If (M.Id "y", M.Num 1, M.Num 2))]))
            ,("match (x) with y => (if y then 1 else 2)", M.Match (M.Id "x", [(Pat.Var "y", M.If (M.Id "y", M.Num 1, M.Num 2))]))
            ,("match x with\n   (Nil)          => 0\n | (Cons y (Nil)) => 1\n | (Cons y ys)    => 2\n",
              M.Match (M.Id "x", [
                         (Pat.Ctor ("Nil", []), M.Num 0)
                        ,(Pat.Ctor ("Cons", [Pat.Var "y", Pat.Ctor ("Nil", [])]), M.Num 1)
                        ,(Pat.Ctor ("Cons", [Pat.Var "y", Pat.Var "ys"]), M.Num 2)
             ]))
           ]
       )
    end

end
