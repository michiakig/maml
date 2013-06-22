structure Lexer : sig

(* TODO: attach line & column numbers *)
datatype t = Num of int
           | Id of string
           | Ctor of string
           | Bool of bool
           | LParen
           | RParen
           | Add
           | Mul
           | Div
           | Sub
           | If
           | Then
           | Else
           | Fn
           | Arrow
           | Let
           | Eqls
           | In
           | Match
           | With
           | Bar

(* TODO: lexFile : filename -> t list *)
val lexStr : string -> t list
val show : t -> string

end =
struct

datatype t = Num of int
           | Id of string
           | Ctor of string
           | Bool of bool
           | LParen
           | RParen
           | Add
           | Mul
           | Div
           | Sub
           | If
           | Then
           | Else
           | Fn
           | Arrow
           | Let
           | Eqls
           | In
           | Match
           | With
           | Bar

fun show (Num n) = "Num " ^ Int.toString n
  | show (Bool b) = "Bool " ^ Bool.toString b
  | show (Id s) = "Id " ^ s
  | show (Ctor s) = "Ctor " ^ s
  | show LParen = "LParen"
  | show RParen = "RParen"
  | show Add = "Add"
  | show Mul = "Mul"
  | show Div = "Div"
  | show Sub = "Sub"
  | show If = "If"
  | show Else = "Else"
  | show Then = "Then"
  | show Fn = "Fn"
  | show Arrow = "Arrow"
  | show Let = "Let"
  | show Eqls = "Eqls"
  | show In = "In"
  | show Match = "Match"
  | show With = "With"
  | show Bar = "Bar"

fun takeWhile p xs =
    let
       fun takeWhile' acc [] = (rev acc, [])
         | takeWhile' acc (all as x::xs) =
           if p x
              then takeWhile' (x::acc) xs
           else (rev acc, all)
    in
       takeWhile' [] xs
    end

fun getDigit chars =
    let
       val (numStr, rest) = takeWhile Char.isDigit chars
    in
       (Int.fromString (String.implode numStr), rest)
    end

fun getWord chars =
    let
       fun notDelim #"+" = false
         | notDelim #"-" = false
         | notDelim #"*" = false
         | notDelim #"/" = false
         | notDelim #"=" = false
         | notDelim #"(" = false
         | notDelim #")" = false
         | notDelim ch = not (Char.isSpace ch)
       val (word, rest) = takeWhile notDelim chars
    in
       (String.implode word, rest)
    end

exception LexicalError of string

(*
 * list-based lexical analyzer, probably pretty slow
 *)
fun lexStr (s : string) : t list =
    let
       fun lexStr' (acc, #"(" :: rest) = lexStr' (LParen :: acc, rest)
         | lexStr' (acc, #")" :: rest) = lexStr' (RParen :: acc, rest)
         | lexStr' (acc, #"+" :: rest) = lexStr' (Add :: acc, rest)
         | lexStr' (acc, #"-" :: rest) = lexStr' (Sub :: acc, rest)
         | lexStr' (acc, #"*" :: rest) = lexStr' (Mul :: acc, rest)
         | lexStr' (acc, #"/" :: rest) = lexStr' (Div :: acc, rest)
         | lexStr' (acc, #"|" :: rest) = lexStr' (Bar :: acc, rest)
         | lexStr' (acc, #"=" :: #">" :: rest) = lexStr' (Arrow :: acc, rest)
         | lexStr' (acc, #"=" :: rest) = lexStr' (Eqls :: acc, rest)
         | lexStr' (acc, all as c :: cs) =
           if Char.isDigit c
              then case getDigit all of
                       (SOME n, rest) => lexStr' ((Num n) :: acc, rest)
                     | (NONE, _) =>
                       raise LexicalError ("error lexing num: " ^ String.implode all)
           else if Char.isSpace c
                   then lexStr' (acc, cs)
                else (case getWord all of
                         ("if", rest) => lexStr' (If :: acc, rest)
                       | ("then", rest) => lexStr' (Then :: acc, rest)
                       | ("else", rest) => lexStr' (Else :: acc, rest)
                       | ("true", rest) => lexStr' (Bool true :: acc, rest)
                       | ("false", rest) => lexStr' (Bool false :: acc, rest)
                       | ("fn", rest) => lexStr' (Fn :: acc, rest)
                       | ("let", rest) => lexStr' (Let :: acc, rest)
                       | ("in", rest) => lexStr' (In :: acc, rest)
                       | ("match", rest) => lexStr' (Match :: acc, rest)
                       | ("with", rest) => lexStr' (With :: acc, rest)
                       | ("", _) =>
                         raise LexicalError ("error lexing: " ^ String.implode all)
                       | (id, rest) => if Char.isUpper (String.sub (id, 0))
                                          then lexStr' ((Ctor id) :: acc, rest)
                                       else lexStr' ((Id id) :: acc, rest))
         | lexStr' (acc, []) = rev acc
    in
       lexStr' ([], String.explode s)
    end
end
