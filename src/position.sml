signature POSITION =
sig
   eqtype t
   val new : t
   val line : t -> int
   val col : t -> int
   val incrCol : t -> t
   val incrLine : t -> t
   val start : 'a -> 'a * t
   val reader : (char,'a) StringCvt.reader -> (char * t,'a * t) StringCvt.reader
end

structure Pos : POSITION =
struct
   type t = {line: int, col: int}
   val new = {col = 1, line = 1}
   fun line {line, col} = line
   fun col {line, col} = col
   fun incrCol {col, line} = {col = col + 1, line = line}
   fun incrLine {col, line} = {col = 1, line = line + 1}
   fun start s = (s, new)
   fun reader rdr =
       fn (s, pos as {col, line}) =>
          case rdr s of
              NONE => NONE
            | SOME (#"\n", s') => SOME ((#"\n", pos), (s', {col=1, line=line+1}))
            | SOME (x, s') => SOME ((x, pos), (s', {col=col+1, line=line}))
   
end
