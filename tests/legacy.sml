(* Our parser requires the lexer to fully tokenize the input and give
 * it a list of tokens. Eventually this will be changed to take a
 * reader (lexer) * and a stream *)
structure Legacy =
struct
   fun lexStr s = Reader.consume (Lexer.make (Pos.reader Reader.string)) (Pos.stream s)
end
