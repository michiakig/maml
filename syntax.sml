structure Syntax =
struct

structure T = TypeInf

datatype expr = Num of int
              | Bool of bool
              | Succ of expr
              | Pred of expr
              | IsZero of expr
              | If of expr * expr * expr
              | App of expr * expr
              | Fun of string * expr
              | Id of string

local
   val id_ = ref 0
in
   fun id () =
       let val id' = !id_
       in (id_ := id' + 1; id')
       end
end

fun makeAst (Num n)           = T.Num (id (), n)
  | makeAst (Bool b)          = T.Bool (id (), b)
  | makeAst (Succ e)          = T.Succ (id (), makeAst e)
  | makeAst (Pred e)          = T.Pred (id (), makeAst e)
  | makeAst (IsZero e)        = T.IsZero (id (), makeAst e)
  | makeAst (If (e1, e2, e3)) = T.If (id (), makeAst e1, makeAst e2, makeAst e3)
  | makeAst (App (e1, e2))    = T.App (id (), makeAst e1, makeAst e2)
  | makeAst (Fun (x, e))      = T.Fun (id (), x, makeAst e)
  | makeAst (Id x)            = T.Id (id (), x)

end
