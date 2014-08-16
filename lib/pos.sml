(* Pos.sml: datatype for storing line and column position in a file.
 * 16 Aug 2014 v0.5 *)

signature POS =
sig
   (* Pos.t is an opaque type representing position in a stream *)
   (* ??? should include raw char count as well as line and col ??? *)
   eqtype t

   (* Pos.new constructs a new value at column and line *)
   val new: int * int -> t

   (* Pos.zero is the start of a file: line 1, column 0 *)
   val zero: t

   (* Select fields *)
   val col: t -> int
   val line: t -> int

   (* Increment fields *)
   val incrCol: t -> t
   val incrLine: t -> t

   (* Advance fields by a value *)
   val advCol: t * int -> t
   val advLine: t * int -> t

   (* Wrap a stream with the zero position *)
   val stream: 'a -> 'a * t
   (* Extract position from positional stream *)
   val getPos: 'a * t -> t

   (* TEMPORARY SHIM *)
   val reader: (char,'a) Reader.t -> (char * t,'a * t) Reader.t

   (* Given a char reader, return a positional char reader *)
   val reader2: (char, 'a) Reader.t -> (char, 'a * t) Reader.t

   val show: t -> string
end

structure Pos =
struct
   type t = {col: int, line: int}

   fun new (col, line) = {col = col, line = line}

   val zero = {col = 1, line = 1}

   fun col      {col, line} = col
   fun line     {col, line} = line

   fun incrCol  {col, line} = {col = col + 1, line = line}
   fun incrLine {col, line} = {col = 1,       line = line + 1}

   fun advCol  ({col, line}, n) = {col = col + n, line = line}
   fun advLine ({col, line}, n) = {col = 0,       line = line + n}

   fun stream s = (s, zero)
   fun getPos (_, p) = p

   fun reader rdr =
       fn (s, p as {col, line}) =>
          case rdr s of
              NONE            => NONE
            | SOME (#"\n", t) => SOME ((#"\n", p), (t, incrLine p))
            | SOME (x,     t) => SOME ((x,     p), (t, incrCol  p))

   fun reader2 rdr =
       fn (s, p) =>
          case rdr s of
              NONE            => NONE
            | SOME (#"\n", t) => SOME (#"\n", (t, incrLine p))
            | SOME (x,     t) => SOME (x,     (t, incrCol  p))

   fun show {col, line} = Int.toString line ^ ":" ^ Int.toString col
end

(* structure Pos :> POS = Pos *)

functor Test () = struct
   local
      open Pos
   in

   val 0 = col  zero
   val 1 = line zero

   val 1 = col  (incrCol  zero)
   val 2 = line (incrLine zero)

   val 0 = col  (incrLine zero)
   val 0 = col  (incrLine (incrCol zero))

   val rdr = reader2 Substring.getc

   (* A fresh positional stream starts at col=0 line=1. *)
   val s = stream (Substring.full "foo")
   val 0 = col  (getPos s)
   val 1 = line (getPos s)

   (* After consuming one char, col incremented by one *)
   val SOME (#"f", (t, p)) = rdr s
   val 1 = col  p
   val 1 = line p

   (* After consuming past a newline, col reset, line advanced *)
   val SOME (_, s) = rdr (stream (Substring.full "x\ny"))
   val SOME (#"\n", s as (_, p)) = rdr s
   val 0 = col  p
   val 2 = line p

   val SOME (#"y", s as (_, p)) = rdr s
   val 1 = col  p
   val 2 = line p

   val 3 = line (new (4, 3))
   val 4 = col (new (4, 3))

   val 5 = col (advCol (zero, 5))
   val 4 = line (advLine (zero, 3))

   end
end
