(* Main entry-point for the compiler. *)
structure Maml = struct
   val fromFile : string -> TextIO.StreamIO.instream =
       TextIO.getInstream o TextIO.openIn

   val rdr   = Pos.reader Reader.streamIO
   val lexer = Lexer.make rdr
   val parse = Parser.parse lexer

   fun main file = Typecheck.inferPgm (parse (Pos.start (fromFile file)))
end
