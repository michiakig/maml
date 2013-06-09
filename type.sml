structure Type : sig

datatype t = Num
           | Bool
           | Arrow of t * t
           | Var of string
           | List of t
val compare : t * t -> order
val show : t -> string
val normalize : t -> t

end = struct

datatype t = Num
           | Bool
           | Arrow of t * t
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
       val s2 = case t2 of
                    Arrow _ => "(" ^ show t2 ^ ")"
                  | _ => show t2
    in
       s1 ^ " -> " ^ s2
    end
and show Num              = "num"
  | show Bool             = "bool"
  | show (Var s)          = "'" ^ s
  | show (Arrow (t1, t2)) = showArrowTyp (t1, t2)
  | show (List t)         = "[" ^ show t ^ "]"

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
       fun normalize' (vars, Bool) = (vars, Bool)
         | normalize' (vars, Num) = (vars, Num)
         | normalize' (vars, (Var tv)) =
           (case StringMap.find (vars, tv) of
                SOME tv' => (vars, Var tv')
              | NONE =>
                let
                   val var = freshVar ()
                in
                   (StringMap.insert (vars, tv, var), Var var)
                end)
         | normalize' (vars, (Arrow (t1, t2))) =
           let
              val (vars', t1') = normalize' (vars, t1)
              val (vars'', t2') = normalize' (vars', t2)
           in
              (vars'', Arrow (t1', t2'))
           end
         | normalize' (vars, List t) =
           let
              val (vars', t') = (normalize' (vars, t))
           in
              (vars', List t')
           end
    in
       #2 (normalize' (StringMap.empty, t))
    end

end
