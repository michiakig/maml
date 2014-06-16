structure Token =
struct

datatype t = Num   of int
           | Id    of string
           | Infix of string
           | Ctor  of string
           | Bool  of bool
           | LParen
           | RParen
           | If
           | Then
           | Else
           | Fn
           | DArrow
           | Let
           | End
           | Eqls
           | In
           | Case
           | Bar

           | Datatype
           | Of
           | Val
           | TypeVar of string
           | TArrow
           | Comma

fun show (Num n)      = "Num "   ^ Int.toString n
  | show (Bool b)     = "Bool "  ^ Bool.toString b
  | show (Id s)       = "Id "    ^ s
  | show (Infix s)    = "Infix " ^ s
  | show (Ctor s)     = "Ctor "  ^ s
  | show LParen       = "LParen"
  | show RParen       = "RParen"
  | show If           = "If"
  | show Else         = "Else"
  | show Then         = "Then"
  | show Fn           = "Fn"
  | show DArrow       = "DArrow"
  | show Let          = "Let"
  | show End          = "End"
  | show Eqls         = "Eqls"
  | show In           = "In"
  | show Case         = "Case"
  | show Bar          = "Bar"

  | show Datatype     = "Datatype"
  | show Of           = "Of"
  | show Val          = "Val"
  | show (TypeVar tv) = "TypeVar " ^ tv
  | show TArrow       = "TArrow"
  | show Comma        = "Comma"

fun eq (Num a,     Num b)     = a = b
  | eq (Bool a,    Bool b)    = a = b
  | eq (Id a,      Id b)      = a = b
  | eq (Infix a,   Infix b)   = a = b
  | eq (Ctor a,    Ctor b)    = a = b
  | eq (TypeVar a, TypeVar b) = a = b

  | eq (LParen,    LParen)    = true
  | eq (RParen,    RParen)    = true
  | eq (If,        If)        = true
  | eq (Else,      Else)      = true
  | eq (Then,      Then)      = true
  | eq (Fn,        Fn)        = true
  | eq (DArrow,    DArrow)    = true
  | eq (Let,       Let)       = true
  | eq (End,       End)       = true
  | eq (Eqls,      Eqls_)     = true
  | eq (In,        In)        = true
  | eq (Case,      Case)      = true
  | eq (Bar,       Bar)       = true
  | eq (Datatype,  Datatype)  = true
  | eq (Of,        Of)        = true
  | eq (Val,       Val)       = true
  | eq (TArrow,    TArrow)    = true
  | eq (Comma,     Comma)     = true

  | eq (_,         _)         = false

end
