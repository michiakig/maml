structure Abstract =
struct

datatype t = Num of int
           | Bool of bool
           | Id of string
           | Add of t * t
           | Mul of t * t
           | Div of t * t
           | Sub of t * t
           | App of t * t (* TODO: curried function application, ie "f g x" ~> "App (App (f, g), x)" *)
           | If of t * t * t
           | Fn of string * t
           | Let of string * t * t (* TODO: multi let *)
           | Match of t * (Pattern.Complex.t * t) list

           | Case of t * (Pattern.Simple.t * t) list
           | Bar of t * t

fun showClause (pat, e) = "(" ^ Pattern.Complex.show pat ^ "=>" ^ show e ^ ")"

and show (Num n) = "Num " ^ Int.toString n
  | show (Bool b) = "Bool " ^ Bool.toString b
  | show (Id s) = "Id " ^ s
  | show (Add (lhs, rhs)) = "Add (" ^ show lhs ^ "," ^ show rhs ^ ")"
  | show (Sub (lhs, rhs)) = "Sub (" ^ show lhs ^ "," ^ show rhs ^ ")"
  | show (Mul (lhs, rhs)) = "Mul (" ^ show lhs ^ "," ^ show rhs ^ ")"
  | show (Div (lhs, rhs)) = "Div (" ^ show lhs ^ "," ^ show rhs ^ ")"
  | show (App (e1, e2)) = "App (" ^ show e1 ^ "," ^ show e2 ^ ")"
  | show (If (e1, e2, e3)) = "If (" ^ show e1 ^ "," ^ show e2 ^ "," ^ show e3 ^ ")"
  | show (Fn (x, e)) = "Fn (" ^ x ^ "," ^ show e ^ ")"
  | show (Let (x, e1, e2)) = "Let (" ^ x ^ "," ^ show e1 ^ "," ^ show e2 ^ ")"
  | show (Match (e, clauses)) = "Match (" ^ show e ^ "," ^ String.concatWith "|" (map showClause clauses) ^ ")"


end
