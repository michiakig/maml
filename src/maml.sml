(* Main entry-point for the compiler. *)
structure Maml = struct
   val fromFile : string -> TextIO.StreamIO.instream =
       TextIO.getInstream o TextIO.openIn

   val rdr        = Pos.reader Reader.streamIO
   fun tokenize s = Reader.consume (Lexer.make rdr) (Pos.start s)
   fun parse s    = Parser.parse (tokenize s)

   fun main file = Typecheck.inferPgm (parse (fromFile file))
end
