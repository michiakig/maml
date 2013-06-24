structure Abstract =
struct

(*
 * monomorphic AST type mostly used for testing
 *)
structure Mono =
struct
   datatype mono = Num of int
                 | Bool of bool
                 | Id of string
                 | Add of mono * mono
                 | Mul of mono * mono
                 | Div of mono * mono
                 | Sub of mono * mono
                 | App of mono * mono
                 | If of mono * mono * mono
                 | Fn of string * mono
                 | Let of string * mono * mono
                 | Match of mono * (Pattern.Complex.t * mono) list

                 | Case of mono * (Pattern.Simple.t * mono) list

local

fun showClause (pat, e) = "(" ^ Pattern.Complex.show pat ^ "=>" ^ show' e ^ ")"

and showClause' (pat, e) = "(" ^ Pattern.Simple.show pat ^ "=>" ^ show' e ^ ")"

and show' (Num (n))            = "Num " ^ Int.toString n
  | show' (Bool (b))           = "Bool " ^ Bool.toString b
  | show' (Id (s))             = "Id " ^ s
  | show' (Add (lhs, rhs))     = "Add (" ^ show' lhs ^ "," ^ show' rhs ^ ")"
  | show' (Sub (lhs, rhs))     = "Sub (" ^ show' lhs ^ "," ^ show' rhs ^ ")"
  | show' (Mul (lhs, rhs))     = "Mul (" ^ show' lhs ^ "," ^ show' rhs ^ ")"
  | show' (Div (lhs, rhs))     = "Div (" ^ show' lhs ^ "," ^ show' rhs ^ ")"
  | show' (App (e1, e2))       = "App (" ^ show' e1 ^ "," ^ show' e2 ^ ")"
  | show' (If (e1, e2, e3))    = "If (" ^ show' e1 ^ "," ^ show' e2 ^ "," ^ show' e3 ^ ")"
  | show' (Fn (x, e))          = "Fn (" ^ x ^ "," ^ show' e ^ ")"
  | show' (Let (x, e1, e2))    = "Let (" ^ x ^ "," ^ show' e1 ^ "," ^ show' e2 ^ ")"
  | show' (Match (e, clauses)) = "Match (" ^ show' e ^ "," ^ String.concatWith "|" (map showClause clauses) ^ ")"

  | show' (Case (e, clauses)) = "Case (" ^ show' e ^ "," ^ String.concatWith "|" (map showClause' clauses) ^ ")"
in
   val show = show'
end

end

(* actual AST type, with polymorphic info *)
datatype 'a t = Num of 'a * int
              | Bool of 'a * bool
              | Id of 'a * string
              | Add of 'a * 'a t * 'a t
              | Mul of 'a * 'a t * 'a t
              | Div of 'a * 'a t * 'a t
              | Sub of 'a * 'a t * 'a t
              | App of 'a * 'a t * 'a t
              | If of 'a * 'a t * 'a t * 'a t
              | Fn of 'a * string * 'a t
              | Let of 'a * string * 'a t * 'a t
              | Match of 'a * 'a t * (Pattern.Complex.t * 'a t) list

              | Case of 'a * 'a t * (Pattern.Simple.t * 'a t) list

fun toMono (Num (_, n))            = Mono.Num n
  | toMono (Bool (_, b))           = Mono.Bool b
  | toMono (Id (_, i))             = Mono.Id i
  | toMono (Add (_, e1, e2))       = Mono.Add (toMono e1, toMono e2)
  | toMono (Sub (_, e1, e2))       = Mono.Sub (toMono e1, toMono e2)
  | toMono (Mul (_, e1, e2))       = Mono.Mul (toMono e1, toMono e2)
  | toMono (Div (_, e1, e2))       = Mono.Div (toMono e1, toMono e2)
  | toMono (If (_, e1, e2, e3))    = Mono.If (toMono e1, toMono e2, toMono e3)
  | toMono (Fn (_, x, e))          = Mono.Fn (x, toMono e)
  | toMono (App (_, e1, e2))       = Mono.App (toMono e1, toMono e2)
  | toMono (Let (_, x, e1, e2))    = Mono.Let (x, toMono e1, toMono e2)
  | toMono (Match (_, e, clauses)) = Mono.Match (toMono e, map (fn (pat, e) => (pat, toMono e)) clauses)
  | toMono (Case (_, e, clauses))  = Mono.Case (toMono e, map (fn (pat, e) => (pat, toMono e)) clauses)

fun getInfo (Num (info, _))       = info
  | getInfo (Bool (info, _))      = info
  | getInfo (Id (info, _))        = info
  | getInfo (Add (info, _, _))    = info
  | getInfo (Sub (info, _, _))    = info
  | getInfo (Mul (info, _, _))    = info
  | getInfo (Div (info, _, _))    = info
  | getInfo (If (info, _, _, _))  = info
  | getInfo (Fn (info, _, _))     = info
  | getInfo (App (info, _, _))    = info
  | getInfo (Let (info, _, _, _)) = info
  | getInfo (Match (info, _, _))  = info
  | getInfo (Case (info, _, _))   = info

local

fun showClause (pat, e) = "(" ^ Pattern.Complex.show pat ^ "=>" ^ show' e ^ ")"

and showClause' (pat, e) = "(" ^ Pattern.Simple.show pat ^ "=>" ^ show' e ^ ")"

and show' (Num (_, n))            = "Num " ^ Int.toString n
  | show' (Bool (_, b))           = "Bool " ^ Bool.toString b
  | show' (Id (_, s))             = "Id " ^ s
  | show' (Add (_, lhs, rhs))     = "Add (" ^ show' lhs ^ "," ^ show' rhs ^ ")"
  | show' (Sub (_, lhs, rhs))     = "Sub (" ^ show' lhs ^ "," ^ show' rhs ^ ")"
  | show' (Mul (_, lhs, rhs))     = "Mul (" ^ show' lhs ^ "," ^ show' rhs ^ ")"
  | show' (Div (_, lhs, rhs))     = "Div (" ^ show' lhs ^ "," ^ show' rhs ^ ")"
  | show' (App (_, e1, e2))       = "App (" ^ show' e1 ^ "," ^ show' e2 ^ ")"
  | show' (If (_, e1, e2, e3))    = "If (" ^ show' e1 ^ "," ^ show' e2 ^ "," ^ show' e3 ^ ")"
  | show' (Fn (_, x, e))          = "Fn (" ^ x ^ "," ^ show' e ^ ")"
  | show' (Let (_, x, e1, e2))    = "Let (" ^ x ^ "," ^ show' e1 ^ "," ^ show' e2 ^ ")"
  | show' (Match (_, e, clauses)) = "Match (" ^ show' e ^ "," ^ String.concatWith "|" (map showClause clauses) ^ ")"

  | show' (Case (_, e, clauses)) = "Case (" ^ show' e ^ "," ^ String.concatWith "|" (map showClause' clauses) ^ ")"

in
   val show = show'
end

end
