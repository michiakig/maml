structure Abstract =
struct

datatype binop = Add | Sub | Mul | Div
(*
 * monomorphic AST type mostly used for testing
 *)
structure Mono =
struct

   datatype mono = Num of int
                 | Bool of bool
                 | Id of string
                 | App of mono * mono
                 | If of mono * mono * mono
                 | Fn of string * mono
                 | Let of string * mono * mono
                 | Match of mono * (Pattern.Complex.t * mono) list
                 | Infix of binop * mono * mono

                 | Case of mono * (Pattern.Simple.t * mono) list

local

fun showClause (pat, e) = "(" ^ Pattern.Complex.show pat ^ "=>" ^ show' e ^ ")"

and showClause' (pat, e) = "(" ^ Pattern.Simple.show pat ^ "=>" ^ show' e ^ ")"

and show' (Num (n))            = "Num " ^ Int.toString n
  | show' (Bool (b))           = "Bool " ^ Bool.toString b
  | show' (Id (s))             = "Id " ^ s
  | show' (App (e1, e2))       = "App (" ^ show' e1 ^ "," ^ show' e2 ^ ")"
  | show' (If (e1, e2, e3))    = "If (" ^ show' e1 ^ "," ^ show' e2 ^ "," ^ show' e3 ^ ")"
  | show' (Fn (x, e))          = "Fn (" ^ x ^ "," ^ show' e ^ ")"
  | show' (Let (x, e1, e2))    = "Let (" ^ x ^ "," ^ show' e1 ^ "," ^ show' e2 ^ ")"
  | show' (Match (e, clauses)) = "Match (" ^ show' e ^ "," ^ String.concatWith "|" (map showClause clauses) ^ ")"

  | show' (Case (e, clauses)) = "Case (" ^ show' e ^ "," ^ String.concatWith "|" (map showClause' clauses) ^ ")"
  | show' (Infix (binop, e1, e2)) = "Infix (" ^ showBinop binop ^ "," ^ show' e1 ^ "," ^ show' e2 ^ ")"
in
   val show = show'
end

end

type pos = {line : int, col : int}



(* actual AST type, with polymorphic info *)
datatype 'a t = Num of 'a * int
              | Bool of 'a * bool
              | Id of 'a * string
              | App of 'a * 'a t * 'a t
              | If of 'a * 'a t * 'a t * 'a t

              (* one info field for bound var, one for self *)
              | Fn of 'a * 'a * string * 'a t
              | Let of 'a * string * 'a t * 'a t
              | Match of 'a * 'a t * (Pattern.Complex.t * 'a t) list
              | Infix of 'a * binop * 'a t * 'a t

              | Case of 'a * 'a t * (Pattern.Simple.t * 'a t) list

fun toMono (Num (_, n))            = Mono.Num n
  | toMono (Bool (_, b))           = Mono.Bool b
  | toMono (Id (_, i))             = Mono.Id i
  | toMono (If (_, e1, e2, e3))    = Mono.If (toMono e1, toMono e2, toMono e3)
  | toMono (Fn (_, _, x, e))       = Mono.Fn (x, toMono e)
  | toMono (App (_, e1, e2))       = Mono.App (toMono e1, toMono e2)
  | toMono (Let (_, x, e1, e2))    = Mono.Let (x, toMono e1, toMono e2)
  | toMono (Match (_, e, clauses)) = Mono.Match (toMono e, map (fn (pat, e) => (pat, toMono e)) clauses)
  | toMono (Infix (_, binop, e1, e2)) = Mono.Infix (binop, toMono e1, toMono e2)
  | toMono (Case (_, e, clauses))  = Mono.Case (toMono e, map (fn (pat, e) => (pat, toMono e)) clauses)

fun getInfo (Num (info, _))       = info
  | getInfo (Bool (info, _))      = info
  | getInfo (Id (info, _))        = info
  | getInfo (If (info, _, _, _))  = info
  (* FIXME: two ids for Fn *)
  | getInfo (Fn (_, info, _, _))  = info
  | getInfo (App (info, _, _))    = info
  | getInfo (Let (info, _, _, _)) = info
  | getInfo (Match (info, _, _))  = info
  | getInfo (Infix (info, _, _, _)) = info
  | getInfo (Case (info, _, _))   = info

local

fun showBinop Add = "Add"
  | showBinop Sub = "Sub"
  | showBinop Mul = "Mul"
  | showBinop Div = "Div"

fun showClause (pat, e) = "(" ^ Pattern.Complex.show pat ^ "=>" ^ show' e ^ ")"

and showClause' (pat, e) = "(" ^ Pattern.Simple.show pat ^ "=>" ^ show' e ^ ")"

and show' (Num (_, n))            = "Num " ^ Int.toString n
  | show' (Bool (_, b))           = "Bool " ^ Bool.toString b
  | show' (Id (_, s))             = "Id " ^ s
  | show' (App (_, e1, e2))       = "App (" ^ show' e1 ^ "," ^ show' e2 ^ ")"
  | show' (If (_, e1, e2, e3))    = "If (" ^ show' e1 ^ "," ^ show' e2 ^ "," ^ show' e3 ^ ")"
  | show' (Fn (_, _, x, e))       = "Fn (" ^ x ^ "," ^ show' e ^ ")"
  | show' (Let (_, x, e1, e2))    = "Let (" ^ x ^ "," ^ show' e1 ^ "," ^ show' e2 ^ ")"
  | show' (Match (_, e, clauses)) = "Match (" ^ show' e ^ "," ^ String.concatWith "|" (map showClause clauses) ^ ")"
  | show' (Infix (_, binop, e1, e2)) = "Infix (" ^ showBinop binop ^ "," ^ show' e1 ^ "," ^ show' e1 ^ ")" 

  | show' (Case (_, e, clauses)) = "Case (" ^ show' e ^ "," ^ String.concatWith "|" (map showClause' clauses) ^ ")"

in
   val show = show'
end

end
