structure AST =
struct
   type pos = {line : int, col : int}

   (*
    * AST for patterns (i.e. in match/case expressions)
    * Two kinds of patterns -- complex appear in source, simple appear in desugared case
    *)
   structure Pattern =
   struct
      structure Complex = (* allows nested patterns *)
      struct
         datatype t = Var of string
                    | Tuple of t list
                    | Ctor of string * t option
         fun show (Var v) = "Var " ^ v
           | show (Ctor (ctor, SOME p)) = "Ctor (" ^ ctor ^ "," ^ show p ^ ")"
           | show (Ctor (ctor, NONE)) = "Ctor " ^ ctor
           | show (Tuple ps) = "Tuple [" ^ Show.list show ps ^ "]"
      end
      structure Simple = (* simple patterns, no nesting *)
      struct
         datatype t = Var of string
                    | Ctor of string * string list
         fun show (Var v) = "Var " ^ v
           | show (Ctor (ctor, vs)) = "Ctor (" ^ ctor ^ ",[" ^ String.concatWith "," vs ^ "])"
      end
   end

   (*
    * Abstract syntax tree for expressions
    *)
   structure Expr =
   struct

   (* actual AST type, with polymorphic info *)
   datatype 'a t = Num of 'a * int
                 | Bool of 'a * bool
                 | Id of 'a * string
                 | App of 'a * 'a t * 'a t
                 | If of 'a * 'a t * 'a t * 'a t

                 (* Fn and Let: one info field for bound var, one for self *)
                 | Fn of 'a * 'a * string * 'a t
                 | Let of 'a * 'a * string * 'a t * 'a t

                 | Case of 'a * 'a t * (Pattern.Complex.t * 'a t) list
                 | Infix of 'a * string * 'a t * 'a t
                 | Tuple of 'a * 'a t list

   fun getInfo (Num (info, _))       = info
     | getInfo (Bool (info, _))      = info
     | getInfo (Id (info, _))        = info
     | getInfo (If (info, _, _, _))  = info
     (* FIXME: two ids for Fn *)
     | getInfo (Fn (_, info, _, _))  = info
     | getInfo (Let (_, info, _, _, _)) = info

     | getInfo (App (info, _, _))    = info
     | getInfo (Case (info, _, _))  = info
     | getInfo (Infix (info, _, _, _)) = info
     | getInfo (Tuple (info, _))     = info

   fun show e =
       let
          fun showClause (pat, e) = "(" ^ Pattern.Complex.show pat ^ "=>" ^ show e ^ ")"
          fun showClause' (pat, e) = "(" ^ Pattern.Simple.show pat ^ "=>" ^ show e ^ ")"
       in
          case e of
              Num (_, n)               => "Num " ^ Int.toString n
            | Bool (_, b)              => "Bool " ^ Bool.toString b
            | Id (_, s)                => "Id " ^ s
            | App (_, e1, e2)          => "App (" ^ show e1 ^ "," ^ show e2 ^ ")"
            | If (_, e1, e2, e3)       => "If (" ^ show e1 ^ "," ^ show e2 ^ "," ^ show e3 ^ ")"
            | Fn (_, _, x, e)          => "Fn (" ^ x ^ "," ^ show e ^ ")"
            | Let (_, _, x, e1, e2)    => "Let (" ^ x ^ "," ^ show e1 ^ "," ^ show e2 ^ ")"
            | Case (_, e, clauses)    => "Case (" ^ show e ^ "," ^ String.concatWith "|" (map showClause clauses) ^ ")"
            | Infix (_, binop, e1, e2) => "Infix (" ^ binop ^ "," ^ show e1 ^ "," ^ show e1 ^ ")"
            | Tuple (_, es) => "Tuple [" ^ String.concatWith "," (map show es) ^ "]"
       end

   fun walk f e =
       case e of
           Num (a, n)               => Num (f a, n)
         | Bool (a, b)              => Bool (f a, b)
         | Id (a, x)                => Id (f a, x)
         | App (a, e1, e2)          => App (f a, walk f e1, walk f e2)
         | If (a, e1, e2, e3)       => If (f a, walk f e1, walk f e2, walk f e3)
         | Fn (a1, a2, x, e)        => Fn (f a1, f a2, x, walk f e)
         | Let (a1, a2, x, e1, e2)  => Let (f a1, f a2, x, walk f e1, walk f e2)
         | Case (a, e1, clauses)    => Case (f a, walk f e1, map (fn (p, e2) => (p, walk f e2)) clauses)
         | Infix (a, binop, e1, e2) => Infix (f a, binop, walk f e1, walk f e2)
         | Tuple (a, es) => Tuple (f a, map (walk f) es)

   end

   (*
    * AST for types, i.e. for type annotations in source code
    *)
   structure Type =
   struct
      datatype 'a t = Var of 'a * string
                    | Con of 'a * string * 'a t list
                    | Arrow of 'a * 'a t * 'a t
                    | Tuple of 'a * 'a t list
                    | Paren of 'a * 'a t
      fun show (Var (_, v)) = "Var (" ^ v ^ ")"
           | show (Con (_, c, ts)) = "Con (" ^ c ^ "," ^ Show.list show ts ^ ")"
           | show (Arrow (_, x, y)) = "Arrow (" ^ show x ^ "," ^ show y ^ ")"
           | show (Tuple (_, ts)) = "Tuple ([" ^ String.concatWith "," (map show ts) ^ "])"
           | show (Paren (_, t)) = "Paren " ^ show t

      fun walk f t =
          case t of
              Var (a, x) => Var (f a, x)
            | Con (a, c, ts) => Con (f a, c, map (walk f) ts)
            | Arrow (a, t1, t2) => Arrow (f a, walk f t1, walk f t2)
            | Tuple (a, ts) => Tuple (f a, map (walk f) ts)
            | Paren (a, t) => Paren (f a, walk f t)

   end

   (*
    * AST for top-level declarations: datatype declarations, value bindings
    *)
   structure Decl =
   struct
      datatype ('a, 'b) t =
               Data of 'b
                       * string list (* zero or more bound type vars *)
                       * string (* type name *)
                       * (string * 'b Type.t option) list (* one or more ctors *)

             | Val of 'b * string * 'a Expr.t
      fun show d =
          let
             fun showCtor (c, NONE)   = "(" ^ c ^ ", NONE)"
               | showCtor (c, SOME t) = "(" ^ c ^ "," ^ Type.show t ^ ")"
          in
             case d of
                 Data (_, tyvars, name, cs) => "Data ([" ^ String.concatWith "," tyvars ^ "]," ^ name ^ ",[" ^ String.concatWith "," (map showCtor cs) ^ "])"
               | Val (_, x, e)      => "Val (" ^ x ^ "," ^ Expr.show e ^ ")"
          end

      fun walk f g d =
          case d of
              Data (b, vars, name, ctors) => Data (g b, vars, name, map (fn (ctor, NONE) => (ctor, NONE) | (ctor, SOME t) => (ctor, SOME (Type.walk g t))) ctors)
            | Val (b, x, e) => Val (g b, x, Expr.walk f e)

   end

   structure Pgm =
   struct
      type ('a, 'b) t = ('a, 'b) Decl.t list
      fun show (p: ('a, 'b) t): string = Show.list Decl.show p
   end

end
