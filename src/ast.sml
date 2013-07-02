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
                    | Ctor of string * t list
         fun show (Var v) = "Var " ^ v
           | show (Ctor (ctor, ps)) = "Ctor (" ^ ctor ^ "," ^ (Show.list show) ps ^ ")"
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

   datatype binop = Add | Sub | Mul | Div

   fun showBinop Add = "Add"
     | showBinop Sub = "Sub"
     | showBinop Mul = "Mul"
     | showBinop Div = "Div"

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
            | Let (_, x, e1, e2)       => "Let (" ^ x ^ "," ^ show e1 ^ "," ^ show e2 ^ ")"
            | Match (_, e, clauses)    => "Match (" ^ show e ^ "," ^ String.concatWith "|" (map showClause clauses) ^ ")"
            | Infix (_, binop, e1, e2) => "Infix (" ^ showBinop binop ^ "," ^ show e1 ^ "," ^ show e1 ^ ")"
            | Case (_, e, clauses)     => "Case (" ^ show e ^ "," ^ String.concatWith "|" (map showClause' clauses) ^ ")"
       end

   end

   (*
    * AST for types, i.e. for type annotations in source code
    *)
   structure Type =
   struct
      datatype 'a t = Var of 'a * string
                    | Con of 'a * string * 'a t
                    | Arrow of 'a * 'a t * 'a t
                    | Tuple of 'a * 'a t list
                    | Paren of 'a * 'a t
      fun show (Var (_, v)) = "Var (" ^ v ^ ")"
           | show (Con (_, c, t)) = "Con (" ^ c ^ "," ^ show t ^ ")"
           | show (Arrow (_, x, y)) = "Arrow (" ^ show x ^ "," ^ show y ^ ")"
           | show (Tuple (_, ts)) = "Tuple ([" ^ String.concatWith "," (map show ts) ^ "])"
           | show (Paren (_, t)) = "Paren " ^ show t
   end

   (*
    * AST for top-level declarations: datatype declarations, value bindings
    *)
   structure Decl =
   struct
      datatype 'a t =
               Data of 'a * string * (string * 'a Type.t option) list
             | Val of 'a * string * 'a Expr.t
      fun show d =
          let
             fun showCtor (c, NONE)   = "(" ^ c ^ ", NONE)"
               | showCtor (c, SOME t) = "(" ^ c ^ "," ^ Type.show t ^ ")"
          in
             case d of
                 Data (_, name, cs) => "Data (" ^ name ^ ",[" ^ String.concatWith "," (map showCtor cs) ^ "])"
               | Val (_, x, e)      => "Val (" ^ x ^ "," ^ Expr.show e ^ ")"
          end
   end

end
