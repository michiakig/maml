structure Constraint =
struct

(*
 * A constraint relates types to types. For instance:
 *    {lhs = Var "x3", rhs = Num }
 *    {lhs = Arrow (Var "a1", Var "b2"), rhs = Arrow (Num, Bool) }
 *)
type t = {lhs: Type.t, rhs: Type.t}

fun show ({lhs, rhs} : t) = "{" ^ Type.show lhs ^ "," ^ Type.show rhs ^ "}"

val compare : t * t -> order =
 fn ({lhs = l , rhs = r}, {lhs = l', rhs = r'}) =>
    case Type.compare (l, l') of
        EQUAL => Type.compare (r, r')
      | ord => ord

local
   structure Set = BinarySetFn(
      struct
         type ord_key = t
         val compare = compare
      end)
   structure Show = SetShowFn(
      structure Set = Set
      structure Show = struct
         type t = t
         val show = show
      end)
in
   structure Set : sig
      include ORD_SET
      val show : set -> string
   end =
   struct
      open Set
      open Show
   end
end

end
