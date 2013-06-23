(*
 * A desugared intermediate form that is close to the object language
 *
 *   - replaces pattern matching with simpler `case` expressions
 *   - other desugarings?
 *)
structure Desugar =
struct

(* simple patterns, no nesting *)
datatype pat = Var of string
             | Ctor of string * string list

datatype t = Num of int
           | Bool of bool
           | Id of string
           | Add of t * t
           | Mul of t * t
           | Div of t * t
           | Sub of t * t
           | App of t * t
           | If of t * t * t
           | Fn of string * t
           | Let of string * t * t
           | Case of t * (pat * t) list

exception UnknownCtor
fun ctors "Cons" = ["Cons", "Nil"]
  | ctors "Nil" = ["Cons", "Nil"]
  | ctors "Branch" = ["Branch", "Leaf"]
  | ctors "Leaf" = ["Branch", "Leaf"]
  | ctors _ = raise UnknownCtor

fun arity "Cons" = 2
  | arity "Nil" = 0
  | arity "Branch" = 2
  | arity "Leaf" = 1
  | arity _ = raise UnknownCtor

local
   val g = ref 0
in
   fun gensym s = s ^ Int.toString (!g) before g := (!g) + 1
end

type eqxn = Pattern.t list * Parser.t
exception Assert of string

(*
 * Given a list of eqxns with patterns all starting with the same ctor,
 * return the eqxns created by reducing the first pattern to its subpatterns,
 * then appending those subpatterns to the rest of the patterns in that equation
 *)
fun subpats (ctor : string, eqxns : eqxn list) : eqxn list =
    let
       fun subpats' (Pattern.Ctor (c, ps') :: ps, e) =
           if c <> ctor
              then raise Assert "subpats: non-matching ctors in eqxns"
           else (ps' @ ps, e)
         | subpats' _ = raise Assert "subpats: pat vars in eqxns"
    in
       map subpats' eqxns
    end

(* fun matchCtor (u::us, qs, def) = *)
(*     Case (u,  *)

(* fun matchClause (ctor, u::us, qs, def) = *)

end
