structure LexerTests =
struct

open QCheck

structure L = Lexer

fun test _ = (
   check (List.getItem, SOME (Show.pair (fn x => x, Show.list L.show)))
         ("lexer", pred (fn (s, toks) => (L.lexStr s) = toks))
         [
           ("0",                        [L.Num 0])
          ,("fn x=>x",                  [L.Fn, L.Id "x", L.Arrow, L.Id "x"])
          ,("fn x => x",                [L.Fn, L.Id "x", L.Arrow, L.Id "x"])
          ,("if 1 then 2 else 3",       [L.If, L.Num 1, L.Then, L.Num 2, L.Else, L.Num 3])
          ,("if foo then bar else baz", [L.If, L.Id "foo", L.Then, L.Id "bar", L.Else, L.Id "baz"])
          ,("let x = 0 in x + x",       [L.Let, L.Id "x", L.Eqls, L.Num 0, L.In, L.Id "x", L.Add, L.Id "x"])
          ,("let x=0 in x + x",         [L.Let, L.Id "x", L.Eqls, L.Num 0, L.In, L.Id "x", L.Add, L.Id "x"])

          ,("match xs with Nil => 0 | Cons x xs => 1", [L.Match, L.Id "xs", L.With, L.Id "Nil", L.Arrow, L.Num 0, L.Bar, L.Id "Cons", L.Id "x", L.Id "xs", L.Arrow, L.Num 1])
          ,("match x with 1 => 1 | 2 => 2", [L.Match, L.Id "x", L.With, L.Num 1, L.Arrow, L.Num 1, L.Bar, L.Num 2, L.Arrow, L.Num 2])
         ])

end
