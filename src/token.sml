structure Token =
struct

datatype 'a t = Num of 'a * int
              | Id of 'a * string
              | Infix of 'a * string
              | Ctor of 'a * string
              | Bool of 'a * bool
              | LParen of 'a
              | RParen of 'a
              | If of 'a
              | Then of 'a
              | Else of 'a
              | Fn of 'a
              | DArrow of 'a
              | Let of 'a
              | End of 'a
              | Eqls of 'a
              | In of 'a
              | Case of 'a
              | Bar of 'a

              | Datatype of 'a
              | Of of 'a
              | Val of 'a
              | TypeVar of 'a * string
              | TArrow of 'a
              | Comma of 'a

fun show (Num (_, n)) = "Num " ^ Int.toString n
  | show (Bool (_, b)) = "Bool " ^ Bool.toString b
  | show (Id (_, s)) = "Id " ^ s
  | show (Infix (_, s)) = "Infix " ^ s
  | show (Ctor (_, s)) = "Ctor " ^ s
  | show (LParen _) = "LParen"
  | show (RParen _) = "RParen"
  | show (If _) = "If"
  | show (Else _) = "Else"
  | show (Then _) = "Then"
  | show (Fn _) = "Fn"
  | show (DArrow _) = "DArrow"
  | show (Let _) = "Let"
  | show (End _) = "End"
  | show (Eqls _) = "Eqls"
  | show (In _) = "In"
  | show (Case _) = "Case"
  | show (Bar _) = "Bar"

  | show (Datatype _) = "Datatype"
  | show (Of _) = "Of"
  | show (Val _) = "Val"
  | show (TypeVar (_, tv)) = "TypeVar " ^ tv
  | show (TArrow _) = "TArrow"
  | show (Comma _) = "Comma"

fun getInfo (Num (info, _))     = info
  | getInfo (Bool (info, _))    = info
  | getInfo (Id (info, _))      = info
  | getInfo (Infix (info, _))   = info
  | getInfo (Ctor (info, _))    = info
  | getInfo (LParen info)       = info
  | getInfo (RParen info)       = info
  | getInfo (If info)           = info
  | getInfo (Else info)         = info
  | getInfo (Then info)         = info
  | getInfo (Fn info)           = info
  | getInfo (DArrow info)       = info
  | getInfo (Let info)          = info
  | getInfo (End info)          = info
  | getInfo (Eqls info)         = info
  | getInfo (In info)           = info
  | getInfo (Case info)         = info
  | getInfo (Bar info)          = info

  | getInfo (Datatype info)     = info
  | getInfo (Of info)           = info
  | getInfo (Val info)          = info
  | getInfo (TypeVar (info, _)) = info
  | getInfo (TArrow info)       = info
  | getInfo (Comma info)        = info

fun eq (Num _, Num _) = true
  | eq (Bool _, Bool _) = true
  | eq (Id (_, a), Id (_, b)) = a = b
  | eq (Infix (_, a), Infix (_, b)) = a = b
  | eq (Ctor (_, a), Ctor (_, b)) = a = b
  | eq (LParen _, LParen _) = true
  | eq (RParen _, RParen _) = true
  | eq (If _, If _) = true
  | eq (Else _, Else _) = true
  | eq (Then _, Then _) = true
  | eq (Fn _, Fn _) = true
  | eq (DArrow _, DArrow _) = true
  | eq (Let _, Let _) = true
  | eq (End _, End _) = true
  | eq (Eqls _, Eqls_) = true
  | eq (In _, In _) = true
  | eq (Case _, Case _) = true
  | eq (Bar _, Bar _) = true
  | eq (Datatype _, Datatype _) = true
  | eq (Of _, Of _) = true
  | eq (Val _, Val _) = true
  | eq (TypeVar _, TypeVar _) = true
  | eq (TArrow _, TArrow _) = true
  | eq (Comma _, Comma _) = true
  | eq _ = false

end
