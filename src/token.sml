structure Token =
struct

datatype 'a t = Num of 'a * int
              | Id of 'a * string
              | Ctor of 'a * string
              | Bool of 'a * bool
              | LParen of 'a
              | RParen of 'a
              | Add of 'a
              | Mul of 'a
              | Div of 'a
              | Sub of 'a
              | If of 'a
              | Then of 'a
              | Else of 'a
              | Fn of 'a
              | Arrow of 'a
              | Let of 'a
              | End of 'a
              | Eqls of 'a
              | In of 'a
              | Match of 'a
              | With of 'a
              | Bar of 'a

              | Datatype of 'a
              | Of of 'a
              | Val of 'a
              | TypeVar of 'a * string
              | Comma of 'a

fun show (Num (_, n)) = "Num " ^ Int.toString n
  | show (Bool (_, b)) = "Bool " ^ Bool.toString b
  | show (Id (_, s)) = "Id " ^ s
  | show (Ctor (_, s)) = "Ctor " ^ s
  | show (LParen _) = "LParen"
  | show (RParen _) = "RParen"
  | show (Add _) = "Add"
  | show (Mul _) = "Mul"
  | show (Div _) = "Div"
  | show (Sub _) = "Sub"
  | show (If _) = "If"
  | show (Else _) = "Else"
  | show (Then _) = "Then"
  | show (Fn _) = "Fn"
  | show (Arrow _) = "Arrow"
  | show (Let _) = "Let"
  | show (End _) = "End"
  | show (Eqls _) = "Eqls"
  | show (In _) = "In"
  | show (Match _) = "Match"
  | show (With _) = "With"
  | show (Bar _) = "Bar"

  | show (Datatype _) = "Datatype"
  | show (Of _) = "Of"
  | show (TypeVar (_, tv)) = "TypeVar " ^ tv
  | show (Comma _) = "Comma"

end
