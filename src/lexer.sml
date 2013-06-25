structure Lexer : sig

(* TODO: attach line & column numbers *)
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

(* TODO: lexFile : filename -> t list *)
val lexStr : string -> {line : int, col: int} t list
val show : 'a t -> string

end =
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
  | show (Eqls _) = "Eqls"
  | show (In _) = "In"
  | show (Match _) = "Match"
  | show (With _) = "With"
  | show (Bar _) = "Bar"

  | show (Datatype _) = "Datatype"
  | show (Of _) = "Of"
  | show (TypeVar (_, tv)) = "TypeVar " ^ tv
  | show (Comma _) = "Comma"

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
       (Int.fromString (String.implode numStr), length numStr, rest)
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
         | notDelim #"," = false
         | notDelim #"|" = false
         | notDelim ch = not (Char.isSpace ch)
       val (word, rest) = takeWhile notDelim chars
    in
       (String.implode word, rest)
    end

exception LexicalError of string

(*
 * list-based lexical analyzer, probably pretty slow
 *)
fun lexStr (s : string) : Abstract.pos t list =
    let
       val line = ref 1
       val col = ref 1
       fun get () = {line = !line, col = !col}
       fun incrLine n = get () before line := !line + n
       fun incrCol n = get () before col := !col + n

       fun lexStr' (acc, #"(" :: rest) = lexStr' (LParen (incrCol 1) :: acc, rest)
         | lexStr' (acc, #")" :: rest) = lexStr' (RParen (incrCol 1):: acc, rest)
         | lexStr' (acc, #"+" :: rest) = lexStr' (Add (incrCol 1) :: acc, rest)
         | lexStr' (acc, #"-" :: rest) = lexStr' (Sub (incrCol 1) :: acc, rest)
         | lexStr' (acc, #"*" :: rest) = lexStr' (Mul (incrCol 1) :: acc, rest)
         | lexStr' (acc, #"/" :: rest) = lexStr' (Div (incrCol 1) :: acc, rest)
         | lexStr' (acc, #"|" :: rest) = lexStr' (Bar (incrCol 1) :: acc, rest)
         | lexStr' (acc, #"=" :: #">" :: rest) = lexStr' (Arrow (incrCol 2) :: acc, rest)
         | lexStr' (acc, #"=" :: rest) = lexStr' (Eqls (incrCol 1) :: acc, rest)
         | lexStr' (acc, #"," :: rest) = lexStr' (Comma (incrCol 1) :: acc, rest)
         | lexStr' (acc, #"\n" :: rest) = (incrLine 1; lexStr' (acc, rest))
         | lexStr' (acc, all as c :: cs) =
           if Char.isDigit c
              then case getDigit all of
                       (SOME n, len, rest) => lexStr' (Num (incrCol len, n) :: acc, rest)
                     | (NONE, _, _) =>
                       raise LexicalError ("error lexing num: " ^ String.implode all)
           else if Char.isSpace c
                   then (incrCol 1; lexStr' (acc, cs))
                else (case getWord all of
                         ("if", rest) => lexStr' (If (incrCol 2) :: acc, rest)
                       | ("then", rest) => lexStr' (Then (incrCol 4) :: acc, rest)
                       | ("else", rest) => lexStr' (Else (incrCol 4) :: acc, rest)
                       | ("true", rest) => lexStr' (Bool (incrCol 4, true) :: acc, rest)
                       | ("false", rest) => lexStr' (Bool (incrCol 5, false) :: acc, rest)
                       | ("fn", rest) => lexStr' (Fn (incrCol 2) :: acc, rest)
                       | ("let", rest) => lexStr' (Let (incrCol 3) :: acc, rest)
                       | ("in", rest) => lexStr' (In (incrCol 2) :: acc, rest)
                       | ("match", rest) => lexStr' (Match (incrCol 5) :: acc, rest)
                       | ("with", rest) => lexStr' (With (incrCol 4) :: acc, rest)
                       | ("datatype", rest) => lexStr' (Datatype (incrCol 8) :: acc, rest)
                       | ("of", rest) => lexStr' (Of (incrCol 2) :: acc, rest)
                       | ("val", rest) => lexStr' (Val (incrCol 3) :: acc, rest)
                       | ("", _) =>
                         raise LexicalError ("error lexing: " ^ String.implode all)
                       | (id, rest) =>
                         if Char.isUpper (String.sub (id, 0))
                            then lexStr' ((Ctor (incrCol (String.size id), id)) :: acc, rest)
                         else if String.sub (id, 0) = #"'"
                            then lexStr' ((TypeVar (incrCol (String.size id), String.substring (id, 1, size id - 1))) :: acc, rest)
                         else lexStr' ((Id (incrCol (String.size id), id)) :: acc, rest))
         | lexStr' (acc, []) = rev acc
    in
       lexStr' ([], String.explode s)
    end
end
