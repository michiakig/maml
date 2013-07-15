structure Type : sig

datatype t = Num
           | Bool
           | Con of string * t list
           | Arrow of t * t
           | Tuple of t list
           | Var of string
           | List of t

val compare : t * t -> order
val show : t -> string
val normalize : t -> t
val fromAST : 'a AST.Type.t -> t

end = struct

datatype t = Num
           | Bool
           | Con of string * t list
           | Arrow of t * t
           | Tuple of t list
           | Var of string
           | List of t

fun fromAST (AST.Type.Var (_, x))        = Var x
  | fromAST (AST.Type.Con (_, c, t))     = Con (c, [fromAST t])
  | fromAST (AST.Type.Arrow (_, t1, t2)) = Arrow (fromAST t1, fromAST t2)
  | fromAST (AST.Type.Tuple (_, ts))     = Tuple (map fromAST ts)
  | fromAST (AST.Type.Paren (_, t))      = fromAST t

local

fun list c =
    let
       fun compare ([], []) = EQUAL
         | compare (x::xs, y::ys) =
           case c (x, y) of
               EQUAL => compare (xs, ys)
             | ord => ord
    in
       compare
    end

in

(* boilerplate comparison stuff *)
fun compare (Num    , Num)  = EQUAL
  | compare (Num    , _)    = GREATER

  | compare (Bool   , Bool) = EQUAL
  | compare (Bool   , Num)  = LESS
  | compare (Bool   , _)    = GREATER

  | compare (Con (c, ts), Con (c', ts')) =
    (case String.compare (c, c') of
         EQUAL => list compare (ts, ts')
       | ord => ord)
  | compare (Con _, Num)  = LESS
  | compare (Con _, Bool) = LESS
  | compare (Con _, _) = GREATER

  | compare (Arrow (t1 , t2), Arrow (t1', t2')) =
    (case compare (t1, t1') of
        EQUAL => compare (t2', t2')
      | ord   => ord)
  | compare (Arrow _, Num)    = LESS
  | compare (Arrow _, Bool)   = LESS
  | compare (Arrow _, _)      = GREATER

  | compare (Tuple ts, Tuple ts') =
    (case Int.compare (length ts, length ts') of
         EQUAL => list compare (ts, ts')
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

end

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
  | show (Var s)          = "'" ^ s
  | show (Con (c, t))     = Show.list show t ^ " " ^ c
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
                  if i >= 26
                     then Char.toString
                             (String.sub (letters, i mod 26)) ^ Int.toString i
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
         | normalize' (Con (c, t)) = Con (c, map normalize' t)
         | normalize' (Var tv) = Var (getVar tv)
         | normalize' (Arrow (t1, t2)) = Arrow (normalize' t1, normalize' t2)
         | normalize' (List t) = List (normalize' t)
         | normalize' (Tuple ts) = Tuple (map normalize' ts)
    in
       normalize' t
    end

end
