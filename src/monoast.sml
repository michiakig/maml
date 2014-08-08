(*
 * monomorphic AST, used to make tests easier to write
 *)
structure MonoAST =
struct
   structure Expr =
   struct
      datatype t = Num of int
                 | Bool of bool
                 | Id of string
                 | App of t * t
                 | If of t * t * t
                 | Fn of string * t
                 | Let of string * t * t
                 | Case of t * (AST.Pattern.Complex.t * t) list
                 | Infix of string * t * t
                 | Tuple of t list

      fun show e =
          let
             fun showClause (pat, e) = "(" ^ AST.Pattern.Complex.show pat ^ "=>" ^ show e ^ ")"
             fun showClause' (pat, e) = "(" ^ AST.Pattern.Simple.show pat ^ "=>" ^ show e ^ ")"
          in
             case e of
                 Num n                 => "Num " ^ Int.toString n
               | Bool b                => "Bool " ^ Bool.toString b
               | Id s                  => "Id " ^ s
               | App (e1, e2)          => "App (" ^ show e1 ^ "," ^ show e2 ^ ")"
               | If (e1, e2, e3)       => "If (" ^ show e1 ^ "," ^ show e2 ^ "," ^ show e3 ^ ")"
               | Fn (x, e)             => "Fn (" ^ x ^ "," ^ show e ^ ")"
               | Let (x, e1, e2)       => "Let (" ^ x ^ "," ^ show e1 ^ "," ^ show e2 ^ ")"
               | Case (e, clauses)    => "Case (" ^ show e ^ "," ^ String.concatWith "|" (map showClause clauses) ^ ")"
               | Tuple es    => "Tuple [" ^ String.concatWith "," (map show es) ^ "]"

               | Infix (binop, e1, e2) => "Infix (" ^ binop ^ "," ^ show e1 ^ "," ^ show e2 ^ ")"
          end

      local
         structure E = AST.Expr
      in
         fun make (e : 'a AST.Expr.t) : t =
             case e of
                 E.Num (_, n)            => Num n
               | E.Bool (_, b)           => Bool b
               | E.Id (_, x)             => Id x
               | E.App (_, f, a)         => App (make f, make a)
               | E.If (_, e1, e2, e3)    => If (make e1, make e2, make e3)
               | E.Fn (_, _, x, e)          => Fn (x, make e)
               | E.Let (_, _, x, e1, e2)    => Let (x, make e1, make e2)
               | E.Case (_, e, clauses) => Case (make e, map (fn (p, e) => (p, make e)) clauses)
               | E.Infix (_, b, e1, e2)  => Infix (b, make e1, make e2)
               | E.Tuple (_, es)  => Tuple (map make es)
      end 
   end

   structure Type =
   struct
      datatype t = Var of string
                 | Con of string * t list
                 | Arrow of t * t
                 | Tuple of t list
                 | Paren of t
      fun show (Var v) = "Var " ^ v
        | show (Con (c, ts)) = "Con (" ^ c ^ "," ^ Show.list show ts ^ ")"
        | show (Arrow (x, y)) = "Arrow (" ^ show x ^ "," ^ show y ^ ")"
        | show (Tuple (ts)) = "Tuple ([" ^ String.concatWith "," (map show ts) ^ "])"
        | show (Paren t) = "Paren " ^ show t

      local
         structure T = AST.Type
      in
         fun make (e : 'a AST.Type.t) : t =
             case e of
                 T.Var (_, x)        => Var x
               | T.Con (_, name, ts)  => Con (name, map make ts)
               | T.Arrow (_, t1, t2) => Arrow (make t1, make t2)
               | T.Tuple (_, ts)     => Tuple (map make ts)
               | T.Paren (_, t)      => Paren (make t)
      end
   end

   structure Decl =
   struct
      datatype t =
               Data of string list * string * (string * Type.t option) list
             | Val of string * Expr.t
      fun show d =
          let
             fun showCtor (c, NONE)   = "(" ^ c ^ ", NONE)"
               | showCtor (c, SOME t) = "(" ^ c ^ "," ^ Type.show t ^ ")"
          in
             case d of
                 Data (tyvars, name, cs) => "Data ([" ^ String.concatWith "," tyvars ^ "]," ^ name ^ ",[" ^ String.concatWith "," (map showCtor cs) ^ "])"
               | Val (x, e)      => "Val (" ^ x ^ "," ^ Expr.show e ^ ")"
          end
      local
         structure D = AST.Decl
      in
         fun make (d : ('a, 'b) AST.Decl.t) : t =
             case d of
                 D.Data (_, tyvars, name, ctors) => Data (tyvars, name, map (fn (c, NONE) => (c, NONE) | (c, SOME t) => (c, SOME (Type.make t))) ctors)
               | D.Val (_, x, e) => Val (x, Expr.make e)
      end
   end

   structure Pgm =
   struct
      type t = Decl.t list
      val show: t Show.t = Show.list Decl.show
      fun make p = map Decl.make p
   end

end
