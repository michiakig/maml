(* Top-level module with a few things used everywhere, should be safe to `open` *)
structure Top = struct
   exception CompilerBug of string
end
