structure LexerTests =
struct

open QCheck
structure T = Token

(*
 * Compare Pos.t with {line: int, col: int}
 *)
fun posEq (pos, {line, col}) =
    Pos.line pos = line andalso Pos.col pos = col

val tokEq: (T.t * Pos.t) * (T.t * {line: int, col: int}) -> bool =
 fn ((t1, p1), (t2, p2)) => T.eq (t1, t2) andalso posEq (p1, p2)

val verbose = false
fun showPos {line: int, col: int} =
    if verbose then
       "{" ^ Int.toString line ^ "," ^ Int.toString col ^ "}"
    else "_"

val show = (Show.pair (Show.string, Show.list (Show.pair (T.show, showPos))))

fun test _ =
   check (List.getItem, SOME show)
         ("lexer", pred (fn (s, toks) => ListPair.allEq tokEq (Legacy.lexStr s, toks)))
         [
           ("0", [(T.Num 0, {line=1,col=1})])

          ,("fn x=>x",
            [
              (T.Fn,     {line=1,col=1}),
              (T.Id "x", {line=1,col=4}),
              (T.DArrow, {line=1,col=5}),
              (T.Id "x", {line=1,col=7})
           ])

          ,("fn (* comment *) x=>x",
            [
              (T.Fn,     {line=1,col=1}),
              (T.Id "x", {line=1,col=18}),
              (T.DArrow, {line=1,col=19}),
              (T.Id "x", {line=1,col=21})
           ])

          ,("fn (* one comment *) (* another comment *) x=>x",
            [
              (T.Fn,     {line=1,col=1}),
              (T.Id "x", {line=1,col=44}),
              (T.DArrow, {line=1,col=45}),
              (T.Id "x", {line=1,col=47})
           ])

          ,("fn (* multi-line \n comment *) x=>x",
            [
              (T.Fn,     {line=1,col=1}),
              (T.Id "x", {line=2,col=13}),
              (T.DArrow, {line=2,col=14}),
              (T.Id "x", {line=2,col=16})
           ])

          ,("fn x => x",
            [
              (T.Fn,     {line=1,col=1}),
              (T.Id "x", {line=1,col=4}),
              (T.DArrow, {line=1,col=6}),
              (T.Id "x", {line=1,col=9})
           ])

          ,("if 1 then 2 else 3",
            [
              (T.If,    {line=1,col=1}),
              (T.Num 1, {line=1,col=4}),
              (T.Then,  {line=1,col=6}),
              (T.Num 2, {line=1,col=11}),
              (T.Else,  {line=1,col=13}),
              (T.Num 3, {line=1,col=18})
           ])

          ,("if foo then bar else baz",
            [
              (T.If,        {line=1,col=1}),
              (T.Id "foo",  {line=1,col=4}),
              (T.Then,      {line=1,col=8}),
              (T.Id  "bar", {line=1,col=13}),
              (T.Else,      {line=1,col=17}),
              (T.Id "baz",  {line=1,col=22})
           ])

          ,("let x = 0 in x + x",
            [
              (T.Let,       {line=1,col=1}),
              (T.Id "x",    {line=1,col=5}),
              (T.Eqls,      {line=1,col=7}),
              (T.Num 0,     {line=1,col=9}),
              (T.In,        {line=1,col=11}),
              (T.Id "x",    {line=1,col=14}),
              (T.Infix "+", {line=1,col=16}),
              (T.Id "x",    {line=1,col=18})
           ])

          ,("let x=0 in x + x",
            [
              (T.Let,       {line=1,col=1}),
              (T.Id "x",    {line=1,col=5}),
              (T.Eqls,      {line=1,col=6}),
              (T.Num 0,     {line=1,col=7}),
              (T.In,        {line=1,col=9}),
              (T.Id "x",    {line=1,col=12}),
              (T.Infix "+", {line=1,col=14}),
              (T.Id "x",    {line=1,col=16})
           ])

          ,("case x of (Nil) => 0 | (Cons y ys) => 1",
            [
              (T.Case,        {line=1,col=1}),
              (T.Id "x",      {line=1,col=6}),
              (T.Of,          {line=1,col=8}),
              (T.LParen,      {line=1,col=11}),
              (T.Ctor "Nil",  {line=1,col=12}),
              (T.RParen,      {line=1,col=15}),
              (T.DArrow,      {line=1,col=17}),
              (T.Num 0,       {line=1,col=20}),
              (T.Bar,         {line=1,col=22}),
              (T.LParen,      {line=1,col=24}),
              (T.Ctor "Cons", {line=1,col=25}),
              (T.Id "y",      {line=1,col=30}),
              (T.Id "ys",     {line=1,col=32}),
              (T.RParen,      {line=1,col=34}),
              (T.DArrow,      {line=1,col=36}),
              (T.Num 1,       {line=1,col=39})
           ])

          ,("case x of 1 => 1 | 2 => 2",
            [
              (T.Case,   {line=1,col=1}),
              (T.Id "x", {line=1,col=6}),
              (T.Of,     {line=1,col=8}),
              (T.Num 1,  {line=1,col=11}),
              (T.DArrow, {line=1,col=13}),
              (T.Num 1,  {line=1,col=16}),
              (T.Bar,    {line=1,col=18}),
              (T.Num 2,  {line=1,col=20}),
              (T.DArrow, {line=1,col=22}),
              (T.Num 2,  {line=1,col=25})
           ])

          ,("datatype 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree",
            [
              (T.Datatype,      {line=1,col=1}),
              (T.TypeVar "a",   {line=1,col=10}),
              (T.Id "tree",     {line=1,col=13}),
              (T.Eqls,          {line=1,col=18}),
              (T.Ctor "Leaf",   {line=1,col=20}),
              (T.Of,            {line=1,col=25}),
              (T.TypeVar "a",   {line=1,col=28}),
              (T.Bar,           {line=1,col=31}),
              (T.Ctor "Branch", {line=1,col=33}),
              (T.Of,            {line=1,col=40}),
              (T.TypeVar "a",   {line=1,col=43}),
              (T.Id "tree",     {line=1,col=46}),
              (T.Infix "*",     {line=1,col=51}),
              (T.TypeVar "a",   {line=1,col=53}),
              (T.Id "tree",     {line=1,col=56})
           ])
         ]

end
