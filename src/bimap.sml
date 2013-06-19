(* monomorphic bidirectional map*)

signature BI_MAP =
sig
   structure K : ORD_KEY
   structure V : ORD_KEY
   type k = K.ord_key
   type v = V.ord_key
   type map
   val empty : map
   val insert : map * k * v -> map
   val findK : map * k -> v option
   val findV : map * v -> k option
   val removeK : map * k -> map * v
   val removeV : map * v -> map * k
   val numItems : map -> int
end

functor BiMapFn (structure Key : ORD_KEY structure Val : ORD_KEY) : BI_MAP =
struct
   structure K = Key
   structure V = Val
   structure KMap = BinaryMapFn(K)
   structure VMap = BinaryMapFn(V)
   type k = K.ord_key
   type v = V.ord_key
   type map = v KMap.map * k VMap.map
   val empty = (KMap.empty, VMap.empty)
   fun insert ((km, vm), k, v) = (KMap.insert (km, k, v), VMap.insert (vm, v, k))
   fun findK ((km, vm), k) = KMap.find (km, k)
   fun findV ((km, vm), v) = VMap.find (vm, v)
   fun removeK ((km, vm), k) =
       let
          val (km', v) = KMap.remove (km, k)
          val (vm', _) = VMap.remove (vm, v)
       in
          ((km', vm'), v)
       end
   fun removeV ((km, vm), v) =
       let
          val (vm', k) = VMap.remove (vm, v)
          val (km', _) = KMap.remove (km, k)
       in
          ((km', vm'), k)
       end
   fun numItems (km, _) = KMap.numItems km
   (* could implement more of the ORD_MAP sig, modified as necessary... *)
end

(* example use
structure IntStringBiMap = BiMapFn (
   structure Key = struct
      type ord_key = int
      val compare = Int.compare
   end
   structure Val = struct
      type ord_key = string
      val compare = String.compare
   end)

val bimap = IntStringBiMap.insert (IntStringBiMap.insert
                                      (IntStringBiMap.empty, 1, "foo"), 2, "bar")
val SOME "foo" = IntStringBiMap.findK (bimap, 1)
val SOME 2 = IntStringBiMap.findV (bimap, "bar")
*)
