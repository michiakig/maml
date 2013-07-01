structure DesugarTests =
struct

open QCheck
open Desugar
structure E = Expr
structure M = Expr.Mono
structure Pat = Pattern.Complex

local
  val eqxn = Show.pair (Show.list Pat.show, E.show)
in
   fun test _ = (
      check (List.getItem, SOME (Show.pair (Show.pair (Show.string, Show.list eqxn),
                                            Show.list eqxn)))
            ("desugar/subpats", pred (fn (inp, out) => out = Desugar.subpats inp))
            [
              (("Cons", [([Pat.Ctor ("Cons", [Pat.Var "x", Pat.Var "xs"])], E.Num ((), 0)),
                         ([Pat.Ctor ("Cons", [Pat.Var "x", Pat.Ctor ("Cons", [Pat.Var "y", Pat.Var "z"])])], E.Num ((), 1))]),
               [([Pat.Var "x", Pat.Var "xs"], E.Num ((), 0)),
                ([Pat.Var "x", Pat.Ctor ("Cons", [Pat.Var "y", Pat.Var "z"])], E.Num ((), 1))])

            , (("Branch", [([Pat.Ctor ("Branch", [Pat.Var "t1", Pat.Var "t2"])], E.Num ((), 0)),
                           ([Pat.Ctor ("Branch", [Pat.Ctor ("Leaf", [Pat.Var "x"]), Pat.Ctor ("Leaf", [Pat.Var "y"])])], E.Num ((), 1))]),
               [([Pat.Var "t1", Pat.Var "t2"], E.Num ((), 0)),
                ([Pat.Ctor ("Leaf", [Pat.Var "x"]), Pat.Ctor ("Leaf", [Pat.Var "y"])], E.Num ((), 1))])
            ])

end

end
