signature PATTERN =
sig

type variable = string
type constructor = string

datatype pattern = Var of variable
                 | Con of constructor * pattern list

(*
 * `arity` returns the arity of a given ctor, e.g.
 * - arity "NIL"
 * val it = 0

 * `constructors` returns a list of all ctors of that ctor's type, e.g.
 * - constructors "CONS"
 * val it = ["NIL", "CONS"]

 * not sure where this information comes from?
 *)
val arity : constructor -> int
val constructors : constructor -> constructor list


datatype expression = Case of variable * clause list
                    | Fatbar of expression * expression
              (* | ... other expressions *)
and clause = Clause of constructor * variable list * expression

(*
 * replace occurences of second var with first var
 *)
val subst : expression * variable * variable -> expression

type equation = pattern list * expression

(*
 * determine if an equation starts with a variable or a ctor
 *)
fun isVar (Var v :: ps, e) = true
  | isVar (Con (c, ps') :: ps, e) = false

fun isCtor q = not (isVar q)

(*
 * get the ctor (if it starts with one)
 *)
fun getCtor (Con (c, ps') :: ps, e) = c

(*
 * given a number, return a variable name
 * (should be replaced with some kind of proper gensym proc?)
 *)
fun makeVar k = "_u" ^ Int.toString k

end

fun tack (x, xss) = (x :: hd xss) :: tl xss

fun partition f [] = []
  | partition f [x] = [[x]]
  | partition f (x :: x' :: xs) =
    if f x = f x'
       then tack (x, (partition f (x' :: xs)))
    else [x] :: partition f (x' :: xs)
