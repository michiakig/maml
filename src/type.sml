structure Type : sig

datatype t = Num
           | Bool
           | Arrow of t * t
           | Tuple of t list
           | Var of string
           | List of t
val compare : t * t -> order
val show : t -> string
val normalize : t -> t

end = struct

datatype t = Num
           | Bool
           | Arrow of t * t
           | Tuple of t list
           | Var of string
           | List of t

(* boilerplate comparison stuff *)
fun compare (Num    , Num)  = EQUAL
  | compare (Num    , _)    = GREATER

  | compare (Bool   , Bool) = EQUAL
  | compare (Bool   , Num)  = LESS
  | compare (Bool   , _)    = GREATER

  | compare (Arrow (t1 , t2), Arrow (t1', t2')) =
    (case compare (t1, t1') of
        EQUAL => compare (t2', t2')
      | ord   => ord)
  | compare (Arrow _, Num)    = LESS
  | compare (Arrow _, Bool)   = LESS
  | compare (Arrow _, _)      = GREATER

  | compare (Tuple ts, Tuple ts') =
    (case Int.compare (length ts, length ts') of
         EQUAL => let fun f ((t1, t2), EQUAL) = compare (t1, t2)
                        | f (_, ord) = ord
                      val xs = ListPair.zip (ts, ts')
                  in foldl f (compare (hd xs)) (tl xs)
                  end
       | ord => ord)
  | compare (Tuple _, Num) = LESS
  | compare (Tuple _, Bool) = LESS
  | compare (Tuple _, Arrow _) = LESS
  | compare (Tuple _, _) = GREATER

  | compare (Var s  , Var s') = String.compare (s, s')
  | compare (Var _  , List _) = GREATER
  | compare (Var _  , _)      = LESS

  | compare (List t, List t') = compare (t, t')
  | compare (List _, _)       = LESS

fun showArrowTyp (t1, t2) =
    let
       val s1 = case t1 of
                    Arrow _ => "(" ^ show t1 ^ ")"
                  | _ => show t1
       (* don't need to wrap result type, -> is left assoc *)
       val s2 = show t2
    in
       s1 ^ " -> " ^ s2
    end
and show Num              = "num"
  | show Bool             = "bool"
  | show (Var s)          = s
  | show (Arrow (t1, t2)) = showArrowTyp (t1, t2)
  | show (List t)         = "[" ^ show t ^ "]"
  | show (Tuple ts)         = "(" ^ String.concatWith "," (map show ts) ^ ")"

fun normalize t =
    let
       val idx = ref 0
       val letters = "abcdefghijklmnopqrstuvwxyz"
       fun freshVar () =
           let
              val i = !idx
              val var =
                  if i >= 52
                     then Char.toString
                             (String.sub (letters, i mod 52)) ^ Int.toString i
                  else Char.toString (String.sub (letters, i))
           in
              var before (idx := i + 1)
           end
       val vars = ref StringMap.empty
       fun getVar tv =
           case StringMap.find (!vars, tv) of
               SOME tv' => tv'
             | NONE =>
               let
                  val var = freshVar ()
               in
                  vars := StringMap.insert (!vars, tv, var)
                ; var
               end

       fun normalize' Bool = Bool
         | normalize' Num = Num
         | normalize' (Var tv) = Var (getVar tv)
         | normalize' (Arrow (t1, t2)) = Arrow (normalize' t1, normalize' t2)
         | normalize' (List t) = List (normalize' t)
         | normalize' (Tuple ts) = Tuple (map normalize' ts)
    in
       normalize' t
    end

end
