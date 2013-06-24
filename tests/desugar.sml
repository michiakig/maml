structure DesugarTests =
struct

open QCheck
open Desugar
structure A = Abstract
structure M = Abstract.Mono
structure Pat = Pattern.Complex

local
  val eqxn = Show.pair (Show.list Pat.show, A.show)
in
   fun test _ = (
      check (List.getItem, SOME (Show.pair (Show.pair (Show.string, Show.list eqxn),
                                            Show.list eqxn)))
            ("desugar/subpats", pred (fn (inp, out) => out = Desugar.subpats inp))
            [
              (("Cons", [([Pat.Ctor ("Cons", [Pat.Var "x", Pat.Var "xs"])], A.Num ((), 0)),
                         ([Pat.Ctor ("Cons", [Pat.Var "x", Pat.Ctor ("Cons", [Pat.Var "y", Pat.Var "z"])])], A.Num ((), 1))]),
               [([Pat.Var "x", Pat.Var "xs"], A.Num ((), 0)),
                ([Pat.Var "x", Pat.Ctor ("Cons", [Pat.Var "y", Pat.Var "z"])], A.Num ((), 1))])

            , (("Branch", [([Pat.Ctor ("Branch", [Pat.Var "t1", Pat.Var "t2"])], A.Num ((), 0)),
                           ([Pat.Ctor ("Branch", [Pat.Ctor ("Leaf", [Pat.Var "x"]), Pat.Ctor ("Leaf", [Pat.Var "y"])])], A.Num ((), 1))]),
               [([Pat.Var "t1", Pat.Var "t2"], A.Num ((), 0)),
                ([Pat.Ctor ("Leaf", [Pat.Var "x"]), Pat.Ctor ("Leaf", [Pat.Var "y"])], A.Num ((), 1))])
            ])

end

end
