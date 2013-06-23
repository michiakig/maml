structure Pattern =
struct

datatype t = Var of string
           | Ctor of string * t list

fun show (Var v) = "Var " ^ v
  | show (Ctor (ctor, ps)) = "Ctor (" ^ ctor ^ "," ^ (Show.list show) ps ^ ")"

end
