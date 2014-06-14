signature LEXER =
sig
   val make : (char * Pos.t, 'a) Reader.t -> (Pos.t Token.t, 'a) Reader.t
   exception LexicalError of string
end

structure Lexer : LEXER =
struct

open Top

fun fst (a, _) = a
fun snd (_, b) = b

(*
 * Extract an integer literal from a positional stream
 *)
fun getInt rdr s =
    let
       val isDigit = Char.isDigit o fst
       val (chars, s') = Reader.takeWhile rdr isDigit s
    in
       if length chars < 1 then
          (NONE, s)
       else
          case Int.fromString (String.implode (map fst chars)) of
              NONE => (NONE, s)
            | SOME n => (SOME (n, snd (hd chars)), s')
    end

(*
 * Drop leading whitespace from a positional stream
 *)
fun skipWS rdr = Reader.dropWhile rdr (Char.isSpace o fst)

(*
 * Extract a keyword or identifier as a string from a positional stream
 *)
fun getWord rdr s =
    let
       fun notDelim #"(" = false
         | notDelim #")" = false
         | notDelim #"," = false
         | notDelim #"|" = false
         | notDelim ch = not (Char.isSpace ch)
       fun isSpecial x = notDelim x andalso Char.isPunct x
       fun isValid x = Char.isAlphaNum x orelse x = #"_"
    in
       case rdr s of
           NONE => (NONE, s)
         | SOME ((x, p), s') =>
           let
              val (chars, s'') =
                  if isSpecial x then
                     Reader.takeWhile rdr (isSpecial o fst) s
                  else Reader.takeWhile rdr (isValid o fst) s
           in
              if length chars < 1 then
                 (NONE, s)
              else (SOME (String.implode (map fst chars), snd (hd chars)), s'')
           end
    end

exception LexicalError of string

(* TODO: needs to be a Set build by previous infix declarations *)
fun isInfix "+" = true
  | isInfix "-" = true
  | isInfix "*" = true
  | isInfix "/" = true
  | isInfix _ = false

fun make (rdr : (char * Pos.t, 'a) Reader.t) : (Pos.t Token.t, 'a) Reader.t =
    fn t =>
       let
          val s = (skipWS rdr t)
       in
          case rdr s of

              NONE => NONE

            (* misc. punctuation *)
            | SOME ((#"(", p), s') => SOME (Token.LParen p, s')
            | SOME ((#")", p), s') => SOME (Token.RParen p, s')
            | SOME ((#"|", p), s') => SOME (Token.Bar p, s')
            | SOME ((#",", p), s') => SOME (Token.Comma p, s')

            (* type variables *)
            | SOME ((#"'", p), s') =>
              (case getWord rdr s' of
                   (SOME ("", _), _) => raise CompilerBug "(Lexer.make) getWord returned empty string"
                 | (NONE, _) => raise LexicalError "Expected type variable after apostrophe"
                 | (SOME (v, _), s'') => SOME (Token.TypeVar (p, v), s''))

            (* integer literals *)
            | SOME ((x, _), s') =>
              if Char.isDigit x then
                 case getInt rdr s of
                     (NONE, _) => raise CompilerBug "(Lexer.make) getInt returned NONE, but stream starts with a digit"
                   | (SOME (n, p), s'') => SOME (Token.Num (p, n), s'')
              else (* all other tokens *)
                 case getWord rdr s of
                     (SOME ("if", p), s'')       => SOME (Token.If p, s'')
                   | (SOME ("then", p), s'')     => SOME (Token.Then p, s'')
                   | (SOME ("else", p), s'')     => SOME (Token.Else p, s'')
                   | (SOME ("true", p), s'')     => SOME (Token.Bool (p, true), s'')
                   | (SOME ("false", p), s'')    => SOME (Token.Bool (p, false), s'')
                   | (SOME ("fn", p), s'')       => SOME (Token.Fn p, s'')
                   | (SOME ("let", p), s'')      => SOME (Token.Let p, s'')
                   | (SOME ("in", p), s'')       => SOME (Token.In p, s'')
                   | (SOME ("end", p), s'')      => SOME (Token.End p, s'')
                   | (SOME ("case", p), s'')     => SOME (Token.Case p, s'')
                   | (SOME ("datatype", p), s'') => SOME (Token.Datatype p, s'')
                   | (SOME ("of", p), s'')       => SOME (Token.Of p, s'')
                   | (SOME ("val", p), s'')      => SOME (Token.Val p, s'')
                   | (SOME ("=", p), s'')        => SOME (Token.Eqls p, s'')
                   | (SOME ("=>", p), s'')       => SOME (Token.DArrow p, s'')
                   | (SOME ("->", p), s'')       => SOME (Token.TArrow p, s'')
                   | (SOME ("", _), _)           => raise CompilerBug ("(Lexer.make) getWord returned empty string, but stream starts with #\"" ^ Char.toString x ^ "\"")
                   | (NONE, _)                   => raise LexicalError "Error lexing"
                   | (SOME (id, p), s'') =>
                     if Char.isUpper (String.sub (id, 0)) then
                        SOME (Token.Ctor (p, id), s'')
                     else if isInfix id then
                        SOME (Token.Infix (p, id), s'')
                     else SOME (Token.Id (p, id), s'')
       end
end
