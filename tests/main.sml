structure Main =
struct
   fun main _ = (
      LexerTests.test ()
    ; ParserTests.test ()
    ; DesugarTests.test ()
    ; TypeInfTests.test ()
    ; OS.Process.success
   )
end
