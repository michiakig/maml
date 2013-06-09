structure StringMap = BinaryMapFn(
   struct
      type ord_key = String.string
      val compare = String.compare
   end)
