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
   val id = ref 0
in
   fun newId () = !id before id := !id + 1
end

fun makeAst (Num n)           = T.Num (newId (), n)
  | makeAst (Bool b)          = T.Bool (newId (), b)
  | makeAst (Succ e)          = T.Succ (newId (), makeAst e)
  | makeAst (Pred e)          = T.Pred (newId (), makeAst e)
  | makeAst (IsZero e)        = T.IsZero (newId (), makeAst e)
  | makeAst (If (e1, e2, e3)) = T.If (newId (), makeAst e1, makeAst e2, makeAst e3)
  | makeAst (App (e1, e2))    = T.App (newId (), makeAst e1, makeAst e2)
  | makeAst (Fun (x, e))      = T.Fun (newId (), x, makeAst e)
  | makeAst (Id x)            = T.Id (newId (), x)

end
