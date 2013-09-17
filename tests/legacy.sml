structure Legacy =
struct
   fun lexStr s = Reader.consume (Lexer.make (Pos.reader Reader.string)) (Pos.start s)
end
