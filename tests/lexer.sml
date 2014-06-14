structure LexerTests =
struct
open QCheck
structure L = Token
(*
 * Compare Pos.t with {line: int, col: int}
 *)
fun posEq (pos, {line, col}) =
    Pos.line pos = line andalso Pos.col pos = col
(*
 * Compare Pos.t Token.t with {line: int, col: int} Token.t
 *)
fun tokEq (t1, t2) =
    posEq (Token.getInfo t1, Token.getInfo t2) andalso Token.eq (t1, t2)
fun test _ = (
   check (List.getItem, SOME (Show.pair (fn x => x, Show.list L.show)))
         ("lexer", pred (fn (s, toks) => ListPair.allEq tokEq (Legacy.lexStr s, toks)))
         [
           ("0", [L.Num ({line=1,col=1}, 0)])

          ,("fn x=>x",
            [
              L.Fn     {line=1,col=1},
              L.Id    ({line=1,col=4}, "x"),
              L.DArrow {line=1,col=5},
              L.Id    ({line=1,col=7}, "x")
           ])

          ,("fn (* comment *) x=>x",
            [
              L.Fn     {line=1,col=1},
              L.Id    ({line=1,col=18}, "x"),
              L.DArrow {line=1,col=19},
              L.Id    ({line=1,col=21}, "x")
           ])

          ,("fn (* one comment *) (* another comment *) x=>x",
            [
              L.Fn     {line=1,col=1},
              L.Id    ({line=1,col=44}, "x"),
              L.DArrow {line=1,col=45},
              L.Id    ({line=1,col=47}, "x")
           ])

          ,("fn (* multi-line \n comment *) x=>x",
            [
              L.Fn     {line=1,col=1},
              L.Id    ({line=2,col=13}, "x"),
              L.DArrow {line=2,col=14},
              L.Id    ({line=2,col=16}, "x")
           ])

          ,("fn x => x",
            [
              L.Fn     {line=1,col=1},
              L.Id    ({line=1,col=4}, "x"),
              L.DArrow {line=1,col=6},
              L.Id    ({line=1,col=9}, "x")
           ])

          ,("if 1 then 2 else 3",
            [
              L.If   {line=1,col=1},
              L.Num ({line=1,col=4}, 1),
              L.Then {line=1,col=6},
              L.Num ({line=1,col=11}, 2),
              L.Else {line=1,col=13},
              L.Num ({line=1,col=18}, 3)
           ])

          ,("if foo then bar else baz",
            [
              L.If   {line=1,col=1},
              L.Id  ({line=1,col=4}, "foo"),
              L.Then {line=1,col=8},
              L.Id  ({line=1,col=13}, "bar"),
              L.Else {line=1,col=17},
              L.Id  ({line=1,col=22}, "baz")
           ])

          ,("let x = 0 in x + x",
            [
              L.Let    {line=1,col=1},
              L.Id    ({line=1,col=5}, "x"),
              L.Eqls   {line=1,col=7},
              L.Num   ({line=1,col=9}, 0),
              L.In     {line=1,col=11},
              L.Id    ({line=1,col=14}, "x"),
              L.Infix ({line=1,col=16}, "+"),
              L.Id    ({line=1,col=18}, "x")
           ])

          ,("let x=0 in x + x",
            [
              L.Let    {line=1,col=1},
              L.Id    ({line=1,col=5}, "x"),
              L.Eqls   {line=1,col=6},
              L.Num   ({line=1,col=7}, 0),
              L.In     {line=1,col=9},
              L.Id    ({line=1,col=12}, "x"),
              L.Infix ({line=1,col=14}, "+"),
              L.Id    ({line=1,col=16}, "x")
           ])

          ,("case x of (Nil) => 0 | (Cons y ys) => 1",
            [
              L.Case   {line=1,col=1},
              L.Id    ({line=1,col=6}, "x"),
              L.Of     {line=1,col=8},
              L.LParen {line=1,col=11},
              L.Ctor  ({line=1,col=12}, "Nil"),
              L.RParen {line=1,col=15},
              L.DArrow {line=1,col=17},
              L.Num   ({line=1,col=20}, 0),
              L.Bar    {line=1,col=22},
              L.LParen {line=1,col=24},
              L.Ctor  ({line=1,col=25}, "Cons"),
              L.Id    ({line=1,col=30}, "y"),
              L.Id    ({line=1,col=32}, "ys"),
              L.RParen {line=1,col=34},
              L.DArrow {line=1,col=36},
              L.Num   ({line=1,col=39},1)
           ])

          ,("case x of 1 => 1 | 2 => 2",
            [
              L.Case   {line=1,col=1},
              L.Id    ({line=1,col=6}, "x"),
              L.Of     {line=1,col=8},
              L.Num   ({line=1,col=11}, 1),
              L.DArrow {line=1,col=13},
              L.Num   ({line=1,col=16}, 1),
              L.Bar    {line=1,col=18},
              L.Num   ({line=1,col=20}, 2),
              L.DArrow {line=1,col=22},
              L.Num   ({line=1,col=25}, 2)
           ])

          ,("datatype 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree",
            [
              L.Datatype {line=1,col=1},
              L.TypeVar ({line=1,col=10}, "a"),
              L.Id      ({line=1,col=13}, "tree"),
              L.Eqls     {line=1,col=18},
              L.Ctor    ({line=1,col=20}, "Leaf"),
              L.Of       {line=1,col=25},
              L.TypeVar ({line=1,col=28}, "a"),
              L.Bar      {line=1,col=31},
              L.Ctor    ({line=1,col=33}, "Branch"),
              L.Of       {line=1,col=40},
              L.TypeVar ({line=1,col=43}, "a"),
              L.Id      ({line=1,col=46}, "tree"),
              L.Infix   ({line=1,col=51}, "*"),
              L.TypeVar ({line=1,col=53}, "a"),
              L.Id      ({line=1,col=56}, "tree")
           ])
         ])

end
