(* Top-level module which will eventually have an entry-point of some kind
 * Right now just has a few things used everywhere, should be safe to `open` *)
structure Maml = struct
   exception CompilerBug of string
end
