structure Pattern =
struct

structure Complex =
struct
   (* allows nested patterns *)
   datatype t = Var of string
              | Ctor of string * t list

   fun show (Var v) = "Var " ^ v
     | show (Ctor (ctor, ps)) = "Ctor (" ^ ctor ^ "," ^ (Show.list show) ps ^ ")"
end

structure Simple =
struct
   (* simple patterns, no nesting *)
   datatype t = Var of string
              | Ctor of string * string list

   fun show (Var v) = "Var " ^ v
     | show (Ctor (ctor, vs)) = "Ctor (" ^ ctor ^ ",[" ^ String.concatWith "," vs ^ "])"
end

end
