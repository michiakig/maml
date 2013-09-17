(*
 * Collection of readers and function on readers and streams
 * See http://spacemanaki.com/blog/2013/08/31/Polymorphic-streams-in-ML/
 *)
structure Reader =
struct

type ('a,'b) t = ('a,'b) StringCvt.reader

val list : ('a, 'a list) t =
    fn [] => NONE
  | (x::xs) => SOME (x, xs)

local
   open String
in
   val string : (char, string) t =
    fn "" => NONE
     | s => SOME (sub (s, 0), substring (s, 1, size s - 1))
end

local
   open Substring
in
   val substring : (char, substring) t = getc
end

val streamIO : (char, TextIO.StreamIO.instream) t = TextIO.StreamIO.input1

(*
 * Given a reader and a stream, consume the entire stream and return a list of the resulting elements
 *)
fun consume (rdr : ('a, 'b) t) s =
    let
       fun consume' acc s =
           case rdr s of
               NONE => rev acc
             | SOME (x, s') => consume' (x::acc) s'
    in
       consume' [] s
    end

fun takeWhile rdr p s =
    let
       fun takeWhile' acc s =
           case rdr s of
               NONE => (rev acc, s)
             | SOME (x, s') => if p x then
                                  takeWhile' (x :: acc) s'
                               else (rev acc, s)
    in
       takeWhile' [] s
    end

fun dropWhile rdr p s =
    let
       fun dropWhile' s =
           case rdr s of
               NONE => s
             | SOME (x, s') => if p x then
                                  dropWhile' s'
                               else s
    in
       dropWhile' s
    end

end
