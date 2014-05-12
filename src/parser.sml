(* Parser for a restricted subset of the ML grammar, adapted from the
 * Definition of Standard ML, appendix B fig 20 p63 *)
structure Parser : sig
   (* TODO: these functions should accept streams and readers, not lists *)
   val parse :     'a Token.t list -> ('a, 'a) AST.Pgm.t
   val parseDecl : 'a Token.t list -> ('a, 'a) AST.Decl.t
   val parseExpr : 'a Token.t list -> 'a AST.Expr.t
   val parseType : 'a Token.t list -> 'a AST.Type.t
   exception SyntaxError of string
end =
struct

   exception SyntaxError of string

   (* flip this to print the grammar productions at each step *)
   val debug = false

   fun log rdr s msg =
       if debug then
          print (msg ^ "(" ^
                 (case rdr s of
                      NONE => ".."
                    | SOME (t, _) => Token.show t)
                 ^ ")\n")
       else ()

   fun expected rdr s msg =
       let
          val got = case rdr s of
                        NONE => "unexpected EOF"
                      | SOME (t, _) => Token.show t
       in
          raise SyntaxError ("expected " ^ msg ^ ", got " ^ got)
       end

   (*
    * Pratt parser for type expressions
    *)
   local

      datatype assoc = Left | Right

      exception NoPrecedence of string
      fun getPrec (Token.Infix (_, "*")) = (60, fn (pos, AST.Type.Tuple (_, xs), y) => AST.Type.Tuple (pos, xs @ [y]) | (pos, x, y) => AST.Type.Tuple (pos, [x, y]), Left)
        | getPrec (Token.TArrow _) = (50, AST.Type.Arrow, Right)
        | getPrec t                = raise NoPrecedence (Token.show t)

      fun isInfix (Token.Infix (_, "*")) = true
        | isInfix (Token.TArrow _) = true
        | isInfix _                = false

   in

      (*
       * accepts a token reader and returns a reader for type expressions
       *)
      fun makeTypeReader (rdr : ('a Token.t, 'b) Reader.t) : ('a AST.Type.t, 'b) Reader.t =
         fn s =>
          let
             (*
              * parse a type sequence as argument to a type ctor, i.e. ('a, 'b) either
              *)
             fun tyseq s acc =
                 (log rdr s "tyseq";
                  case rdr s of
                      SOME (Token.RParen _, s') => (case rdr s' of
                                                        SOME (Token.Id (p, c), s'') => SOME (AST.Type.Con (p, c, rev acc), s'')
                                                      |  _                          => expected rdr s' "tycon following tyseq in type expression")
                    | SOME (Token.Comma _, s') => (case infexp s' 0 of
                                                       SOME (t, s'') => tyseq s'' (t :: acc)
                                                     | NONE          => expected rdr s' "type expression following comma in tyseq")
                    | _ => expected rdr s "comma or right paren")

             (*
              * parse an atomic expression -- var or parenthesized infix
              *)
             and atom s =
                 (log rdr s "atom";
                  case rdr s of
                      SOME (Token.TypeVar v, s') => SOME (AST.Type.Var v, s')
                    | SOME (Token.LParen p, s') =>
                      (case infexp s' 0 of
                           SOME (ty, s'') =>
                           (case rdr s'' of
                                SOME (Token.RParen _, s''') => SOME (AST.Type.Paren (p, ty), s''')
                              | SOME (Token.Comma _, s''')  => tyseq s'' [ty]
                              | _                           => expected rdr s'' "comma or right paren")
                         | NONE => expected rdr s' "type expression after left paren")
                    | SOME (Token.Id (p, c), s') => SOME (AST.Type.Con (p, c, []), s')
                    | _                          => expected rdr s "type variable, left paren, or type constructor (ident)")

             (*
              * parse an infix expression
              *)
             and infexp s prec =
                 (log rdr s "infexp";
                  let
                     fun infexp' s prec lhs =
                         (log rdr s "infexp'";
                          case rdr s of
                              SOME (Token.Id (p, c), s') => (infexp' s' prec (AST.Type.Con (p, c, [lhs])))
                            | SOME (t, s') =>
                              if isInfix t then
                                 let val (prec', ctor, assoc) = getPrec t
                                 in
                                    if prec < prec' then
                                       let
                                          val prec'' = case assoc of Left => prec' | Right => prec' - 1
                                       in
                                          case infexp s' prec'' of
                                              SOME (ty, s'') => infexp' s'' prec (ctor (Token.getInfo t, lhs, ty))
                                            | _ => expected rdr s' "right hand side in type expression"
                                       end
                                    else SOME (lhs, s)
                                 end
                              else SOME (lhs, s)
                            | NONE => SOME (lhs, s))
                  in
                     case atom s of
                         NONE => NONE
                       | SOME (ast, s') => infexp' s' prec ast
                  end)
          in
             infexp s 0
          end
   end

(* check if token is in FIRST(atexp) *)
fun FIRSTatexp (Token.Id (pos, _))   = true
  | FIRSTatexp (Token.Num (pos, _))  = true
  | FIRSTatexp (Token.Bool (pos, _)) = true
  | FIRSTatexp (Token.Let pos)       = true
  | FIRSTatexp (Token.LParen pos)    = true
  | FIRSTatexp _                 = false

fun isBinop (Token.Infix (_, "+")) = true
  | isBinop (Token.Infix (_, "-")) = true
  | isBinop (Token.Infix (_, "*")) = true
  | isBinop (Token.Infix (_, "/")) = true
  | isBinop _ = false

fun getBinop (Token.Infix (_, oper)) = oper
  | getBinop _ = raise Match

structure Expr = AST.Expr

(*
 * accepts a token reader and returns a reader for expressions
 *)
fun makeExprReader (rdr : ('a Token.t, 'b) Reader.t) : ('a AST.Expr.t, 'b) Reader.t =
   fn s =>
    let
       val rest = ref s

       fun has () = case rdr (!rest) of
                        NONE => false
                      | SOME _ => true

       fun adv () = rest := (case rdr (!rest) of
                                 NONE => raise Empty
                               | SOME (_, s) => s)

       fun peek () = case rdr (!rest) of
                         NONE => raise Empty
                       | SOME (t, _) => t

       fun next () = peek () before adv ()

       fun getNext () = if has () then SOME (next ()) else NONE

       fun err s = raise SyntaxError ("err " ^ s)
       fun expected s t = raise SyntaxError ("expected " ^ s ^ ", got " ^ Token.show t)

       fun getPrec () : int =
           (if has ()
               then (case peek () of
                         Token.Infix (_, "+") => 1
                       | Token.Infix (_, "-") => 1
                       | Token.Infix (_, "*") => 2
                       | Token.Infix (_, "/") => 2
                       | _ => 0)
            else 0)


       fun expr () : 'a Expr.t =
           (log rdr (!rest) "expr";
            case peek () of
                Token.If pos =>
                (adv ()
                ; let val e1 = expr ()
                  in case peek () of
                         Token.Then _ => (adv ()
                                     ; let val e2 = expr ()
                                       in case peek () of
                                              Token.Else _ => (adv ()
                                                          ; Expr.If (pos, e1, e2, expr ()))
                                            | t => expected "else" t
                                       end)
                       | t => expected "then" t
                  end)
              | Token.Fn pos =>
                (adv ()
                ; case peek () of
                      Token.Id (pos', x) => (adv ()
                                     ; case peek () of
                                           Token.DArrow _ => (adv ()
                                                        (* FIXME: two ids for Fn *)
                                                        ; Expr.Fn (pos', pos, x, expr ()))
                                         | t => expected "=>" t)
                    | t => err ("expected formal arg in fn expr, got " ^ Token.show t))

              | Token.Case pos =>
                (adv ()
                ; let val e1 = expr ()
                  in case peek () of
                         Token.Of _ => (adv ()
                                     ; Expr.Case (pos, e1, clauses ()))
                       | t => expected "of" t
                  end)

              | _ => infexp 0)

       and clauses () : (AST.Pattern.Complex.t * 'a Expr.t) list =
           (log rdr (!rest) "clauses";
            let
               val pat = pattern ()
            in
               (case peek () of
                    Token.DArrow _ => (adv (); (pat, expr ()) :: clauses' ())
                  | t => expected "=>" t)
            end)

       and clauses' () : (AST.Pattern.Complex.t * 'a Expr.t) list =
           (log rdr (!rest) "clauses'"
           ; if has ()
                then case peek () of
                         Token.Bar _ => (adv () ; clauses ())
                       | _ => []
             else [])

       and pattern () : AST.Pattern.Complex.t =
           (log rdr (!rest) "pattern"
           ; case peek () of
                 Token.Id (_, x) => (adv (); AST.Pattern.Complex.Var x)
               | Token.Ctor (_, c) => (adv ();
                                       if has ()
                                          then case peek () of
                                                   Token.Id _ => AST.Pattern.Complex.Ctor (c, SOME (pattern ()))
                                                 | Token.LParen _ => AST.Pattern.Complex.Ctor (c, SOME (pattern ()))
                                                 | _ => AST.Pattern.Complex.Ctor (c, NONE)
                                       else AST.Pattern.Complex.Ctor (c, NONE))
               | Token.LParen _ => (adv ()
                                   ; let val p = pattern ()
                                     in
                                        case peek () of
                                            Token.Comma _ => AST.Pattern.Complex.Tuple (p :: patterns ())
                                          | Token.RParen _ => (adv (); p)
                                          | t => expected "comma or ) in pattern" t
                                     end)
               | t => expected "var, tuple, or ctor application in pattern" t)

       and patterns () : AST.Pattern.Complex.t list =
           (log rdr (!rest) "patterns"
           ; if has ()
                then case peek () of
                         Token.Comma _ => (adv (); pattern () :: patterns ())
                       | Token.RParen _ => (adv (); [])
                       | _ => []
             else [])

       and infexp (prec : int) : 'a Expr.t =
           (log rdr (!rest) "infexp";
            let
               val lhs = appexp ()
            in
               if has () andalso isBinop (peek ())
                  then infexp' (prec, lhs)
               else lhs
            end)

       and infexp' (prec : int, lhs : 'a Expr.t) : 'a Expr.t =
           (log rdr (!rest) "infexp'";
            let
               val prec' = getPrec ()
            in
               if prec < prec'
                  then let val t = next ()
                           (* TODO: check if t is a binop *)
                           val lhs = Expr.Infix (Expr.getInfo lhs, getBinop t, lhs,
                                              infexp prec')
                       in infexp' (prec, lhs)
                       end
               else lhs
            end)

       and tuple () : 'a Expr.t list =
           (log rdr (!rest) "tuple";
            case peek () of
                Token.Comma _ => (adv (); let val e = expr ()
                                          in e :: tuple ()
                                          end)
              | Token.RParen _ => (adv (); [])
              | t => expected "comma or )" t)

       and atexp () : 'a Expr.t =
           (log rdr (!rest) "atexp";
            case peek () of
                Token.Let pos =>
                (adv ()
                ; case peek () of
                      Token.Val _ =>
                      (adv ()
                      ; case peek () of
                            Token.Id (pos', x) =>
                            (adv ()
                            ; case peek () of
                                  Token.Eqls _ =>
                                  (adv ()
                                  ; let val bound = expr ()
                                    in case peek () of
                                           Token.In _ =>
                                           (adv ();
                                            let val body = expr ()
                                            in case peek () of
                                                   Token.End _ => (adv (); Expr.Let (pos', pos, x, bound, body))
                                                 | t => expected "end" t
                                            end)
                                         | t => expected "in" t
                                    end)
                                | t => expected "=" t)
                          | t => err ("expected bound var in let expr, got " ^ Token.show t))
                    | t => expected "val" t)
              | Token.Num (pos, n) => (adv (); Expr.Num (pos, n))
              | Token.Bool (pos, b) => (adv (); Expr.Bool (pos, b))
              | Token.Id (pos, s) => (adv (); Expr.Id (pos, s))

              (* in the context of an atexp, parse a Ctor as an Ident. *)
              | Token.Ctor (pos, s) => (adv (); Expr.Id (pos, s))

              | Token.LParen pos =>
                (adv ();
                 let
                    val e = expr ()
                 in
                    case peek () of
                        Token.RParen _ => (adv (); e)
                      | Token.Comma _ => Expr.Tuple (pos, e :: tuple ())
                      | t => expected "comma or )" t
                 end)
              | t => expected "let, id or constant" t)

       (*
        * lhs is the left hand side of the (potential) application
        *)
       and appexp' (lhs : 'a Expr.t) : 'a Expr.t =
           (log rdr (!rest) "appexp'";
            if has () andalso FIRSTatexp (peek ())
               then appexp' (Expr.App (Expr.getInfo lhs, lhs, atexp ()))
            else lhs)

       and appexp () : 'a Expr.t =
           (log rdr (!rest) "appexp";
            appexp' (atexp ()))

    in
       SOME (expr (), !rest)
    end

(*
 * accepts a token reader and returns a reader for declarations
 *)
fun makeDeclReader (rdr : ('a Token.t, 'b) Reader.t) : (('a, 'a) AST.Decl.t, 'b) Reader.t =
   fn s =>
    let
       val rest = ref s

       fun has () = case rdr (!rest) of
                        NONE => false
                      | SOME _ => true

       fun adv () = rest := (case rdr (!rest) of
                                 NONE => raise Empty
                               | SOME (_, s) => s)

       fun peek () = case rdr (!rest) of
                         NONE => raise Empty
                       | SOME (t, _) => t

       fun next () = peek () before adv ()

       fun getNext () = if has () then SOME (next ()) else NONE

       fun err s = raise SyntaxError ("err " ^ s)
       fun expected s t = raise SyntaxError ("expected " ^ s ^ ", got " ^ Token.show t)

       fun log s =
           let val t = if has () then Token.show (peek ()) else ".."
           in if debug
                 then print (s ^ "(" ^ t ^ ")\n")
              else ()
           end

       fun ctor () : string * 'a AST.Type.t option =
           (log "ctor";
            case peek () of
                Token.Ctor (_, name) => (adv ();
                                         if has ()
                                         then
                                            case peek () of
                                                Token.Of _ => (adv ();
                                                               case (makeTypeReader rdr) (!rest) of
                                                                   NONE => raise Empty
                                                                 | SOME (typ, rest') => (rest := rest' ; (name, SOME typ)))
                                              | _ => (name, NONE)
                                         else (name, NONE))
              | t => expected "ctor in datatype decl" t)

       and ctors' () : (string * 'a AST.Type.t option) list =
           (log "ctors'";
            if has ()
            then
               case peek () of
                   Token.Bar _ => (adv (); ctor () :: ctors' ())
                 | _ => []
            else [] )

       and ctors () : (string * 'a AST.Type.t option) list =
           (log "ctors";
            ctor () :: ctors' ())

       (* parse a datatype declaration *)
       and data pos : ('a, 'a) AST.Decl.t =
           let
              (* parse the rest of a datatype declaration, after `datatype 'a` or `datatype ('a, 'a)` *)
              fun data' tyvars =
                  case peek () of
                      Token.Id (_, id) => (adv (); case peek () of
                                                       Token.Eqls _ => (adv (); AST.Decl.Data (pos, tyvars, id, ctors ()))
                                                     | t => expected "= in datatype decl" t)
                    | t => expected "identifier in datatype declaration" t

              fun tyvars () =
                  case peek () of
                      Token.Comma _ => (adv (); case peek () of
                                                    Token.TypeVar (_, tyvar) => (adv (); tyvar :: tyvars ())
                                                  | t => expected "type variable in datatype declaration" t)
                    | Token.RParen _ => (adv (); [])
                    | t => expected "comma or ) in datatype declaration" t
           in
              log "data"
            ; case peek () of
                  Token.TypeVar (_, tyvar) => (adv (); data' [tyvar])
                | Token.LParen _ => (adv (); case peek () of
                                                 Token.TypeVar (_, tyvar) => (adv (); data' (tyvar :: tyvars ()))
                                               | t => expected "type variable in datatype declaration" t)
                | _ => data' []
           end

       and decl () : ('a, 'a) AST.Decl.t =
           (log "decl";
            case peek () of
                Token.Datatype pos => (adv ()
                                      ; data pos)
              | Token.Val pos => (adv ();
                                  case peek () of
                                      Token.Id (_, id) => (adv (); case peek () of
                                                                       Token.Eqls _ => (adv ();
                                                                                        case makeExprReader rdr (!rest) of
                                                                                            NONE => raise Empty
                                                                                          | SOME (e, rest') => (rest := rest' ; AST.Decl.Val (pos, id, e)))
                                                                     | t => expected "= in val decl" t)
                                    | t => expected "ident in val decl" t)
              | t => expected "datatype or val in top-level decl" t)
    in
       SOME (decl (), !rest)
    end

fun parseExpr (toks : 'a Token.t list) : 'a AST.Expr.t =
    case makeExprReader Reader.list toks of
        NONE => raise Empty
      | SOME (ast, _) => ast

fun parseType (toks : 'a Token.t list) : 'a AST.Type.t =
    case makeTypeReader Reader.list toks of
        NONE => raise Empty
      | SOME (ast, _) => ast

fun parseDecl (toks : 'a Token.t list) : ('a, 'a) AST.Decl.t =
    case makeDeclReader Reader.list toks of
        NONE => raise Empty
      | SOME (ast, _) => ast

fun parse (toks : 'a Token.t list) : ('a, 'a) AST.Pgm.t =
    let
       val parseDecl' = makeDeclReader Reader.list

       fun parse' toks =
           (case parseDecl' toks of
                NONE => []
              | SOME (ast, rest) => ast :: parse' rest) handle Empty => []
    in
       parse' toks
    end

end
