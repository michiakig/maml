structure Show =
   struct
      type 'a t = 'a -> string
      val unit: unit t = fn _ => "()"
      val int: int t = Int.toString
      val word: word t = Word.toString
      val char: char t = Char.toString
      val real: real t = Real.toString
      val string: string t = fn s => "\"" ^ s ^ "\""
      val bool: bool t = Bool.toString
      local
         fun interleave l i =
             let
                fun recur [] acc = rev acc
                  | recur (x :: []) acc = recur [] (x :: acc)
                  | recur (x :: xs) acc = recur xs (i :: x :: acc)
             in
                recur l []
             end
      in
         val list: 'a t -> 'a list t =
          fn show => fn xs => "[" ^ concat (interleave (map show xs) ",") ^ "]"
      end
      val option: 'a t -> 'a option t =
       fn show => fn NONE => "NONE" | (SOME x) => "SOME " ^ show x

      val pair: 'a t * 'b t -> ('a * 'b) t =
       fn (showa,showb) => fn (a,b) => "(" ^ showa a ^ "," ^ showb b ^ ")"

      val triple: 'a t * 'b t * 'c t -> ('a * 'b * 'c) t =
       fn (showa,showb,showc) => fn (a,b,c) => "(" ^ showa a ^ ","
                                               ^ showb b ^ ","
                                               ^ showc c ^ ")"

      val sq: 'a t -> ('a * 'a) t =
       fn (show) => fn (a,a') => "(" ^ show a ^ "," ^ show a' ^ ")"
   end

signature SHOW =
   sig
      type t
      val show : t -> string
   end

functor SetShowFn(structure Set: ORD_SET
                  structure Show: SHOW
                  sharing type Set.Key.ord_key = Show.t): SHOW =
   struct
      type t = Set.set

      fun show s =
          let
             val items = Set.listItems s
             val strs = map Show.show items
             fun interleave l i =
                 let
                    fun recur [] acc = rev acc
                      | recur (x :: []) acc = recur [] (x :: acc)
                      | recur (x :: xs) acc = recur xs (i :: x :: acc)
                 in
                    recur l []
                 end
          in
             "{" ^ String.concat (interleave strs ",") ^ "}"
          end
   end
