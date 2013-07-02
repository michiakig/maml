structure Main =
struct
   fun main _ = (
      ParserTests.test ()
    ; OS.Process.success
   )
end
