(* ML grammar, adapted from the Definition of Standard ML, appendix B fig 20 p63

 atexp    ::= constant
              <value id>
              let val <id> = <exp> in <exp> end
              ( <exp> )
              <... records, selectors, tuples, sequences, lists ...>

 appexp   ::= <atexp> <appexp'>
 appexp'  ::= <atexp> <appexp'>
              <e>

 infexp   ::= <appexp> <infexp'>
 infexp'  ::= id <infexp> <infexp'>
              <e>

 exp      ::= <infexp>
              if <exp> then <exp> else <exp>
              match <exp> with <clauses>
              fn <id> => <exp>

 clauses  ::= <pattern> => <exp> <clauses'>
 clauses' ::= | <clauses>
 clauses'     <e>

 pattern  ::= <id>
 pattern      (<ctor> <pattern'>)
 pattern' ::= <pattern> <pattern'>
 pattern'     <e>

 *)

structure Parser (*  : sig  *)

(* val parseExpr : 'a Token.t list -> 'a AST.Expr.t *)

(* end  *)=
struct

exception SyntaxError of string

   (*
    * Pratt parser for type expressions
    *)
   local

      datatype assoc = Left | Right

      exception NoPrecedence of string
      fun getPrec (Token.Mul _)    = (60, fn (pos, AST.Type.Tuple (_, xs), y) => AST.Type.Tuple (pos, xs @ [y]) | (pos, x, y) => AST.Type.Tuple (pos, [x, y]), Left)
        | getPrec (Token.TArrow _) = (50, AST.Type.Arrow, Right)
        | getPrec t                = raise NoPrecedence (Token.show t)

      fun isInfix (Token.Mul _)    = true
        | isInfix (Token.TArrow _) = true
        | isInfix _                = false

   in

      (*
       * given a list of tokens, parse a single type expression, and return the unconsumed input
       *)
      fun parseType (ts : 'a Token.t list) : 'a AST.Type.t * 'a Token.t list =
          let
             val rest = ref ts
             fun has () = not (null (!rest))
             fun peek () = case !rest of [] => NONE | t :: _ => SOME t
             fun unsafe () = hd (!rest)
             fun eat () = rest := tl (!rest)
             fun next () = peek () before eat ()

             val debug = true
             fun log s =
                 if debug
                    then (print (s ^ "(" ^
                                 (case peek () of
                                      NONE => ".."
                                    | SOME t => Token.show t)
                                 ^ ")")
                         ; print "\n")
                 else ()
             fun expected s t = raise SyntaxError ("expected " ^ s ^
                                                   ", got " ^ Token.show t)
             (*
              * parse an atomic expression -- var or parenthesized infix
              *)
             fun atom () : 'a AST.Type.t =
                 (log "atom";
                  case peek () of
                      SOME (Token.TypeVar v) => (eat (); AST.Type.Var v)
                    | SOME (Token.LParen pos)  => (eat (); case peek () of
                                                               SOME (Token.RParen _) => AST.Type.Paren (pos, infexp 0)
                                                             | SOME t => expected "RParen" t
                                                             | NONE => raise SyntaxError "unexpected EOF")
                    | SOME t             => expected "TypeVar or LParen" t
                    | NONE               => raise SyntaxError "unexpected EOF")

             (*
              * parse an infix expression
              *)
             and infexp (prec : int) : 'a AST.Type.t =
                 (log "infexp";
                  let
                     fun infexp' (prec : int, lhs : 'a AST.Type.t) : 'a AST.Type.t =
                         (log "infexp'";
                          case peek () of
                              SOME (Token.Id (pos, c)) => (eat (); infexp' (prec, AST.Type.Con (pos, c, lhs)))
                            | SOME t =>
                              if isInfix t
                              then
                                 let val (prec', ctor, assoc) = getPrec t
                                 in
                                    if prec < prec'
                                    then let val _ = eat ();
                                             val prec'' = case assoc of Left => prec' | Right => prec' - 1
                                             val lhs = ctor (Token.getInfo t, lhs, infexp prec'')
                                         in infexp' (prec, lhs)
                                         end
                                    else lhs
                                 end
                              else lhs
                            | _ => lhs)
                  in
                     infexp' (prec, atom ())
                  end)
          in
             (infexp 0, !rest)
          end
   end

(* check if token is in FIRST(atexp) *)
fun FIRSTatexp (Token.Id (pos, _))   = true
  | FIRSTatexp (Token.Num (pos, _))  = true
  | FIRSTatexp (Token.Bool (pos, _)) = true
  | FIRSTatexp (Token.Let pos)       = true
  | FIRSTatexp (Token.LParen pos)    = true
  | FIRSTatexp _                 = false

fun isBinop (Token.Add _) = true
  | isBinop (Token.Sub _) = true
  | isBinop (Token.Mul _) = true
  | isBinop (Token.Div _) = true
  | isBinop _ = false

fun getBinop (Token.Add _) = AST.Expr.Add
  | getBinop (Token.Sub _) = AST.Expr.Sub
  | getBinop (Token.Mul _) = AST.Expr.Mul
  | getBinop (Token.Div _) = AST.Expr.Div
  | getBinop _ = raise Match

structure Expr = AST.Expr

(*
 * Given a list of tokens, parse one expression
 *)
fun parse (toks : 'a Token.t list) : 'a AST.Decl.t =
    let
       val rest = ref toks
       fun has () = not (null (!rest))
       fun adv () = rest := tl (!rest)
       fun next () = hd (!rest) before adv ()
       fun getNext () = if has () then SOME (next ()) else NONE
       fun peek () = hd (!rest)
       fun err s = raise SyntaxError ("err " ^ s)
       fun expected s t = raise SyntaxError ("expected " ^ s ^ ", got " ^ Token.show t)

       fun getPrec () : int =
           (if has ()
               then (case peek () of
                         Token.Add _ => 1
                       | Token.Sub _ => 1
                       | Token.Mul _ => 2
                       | Token.Div _ => 2
                       | _ => 0)
            else 0)

       (* flip this to print the grammar productions at each step *)
       val debug = true
       fun log s =
           let val t = if has () then Token.show (peek ()) else ".."
           in if debug
                 then print (s ^ "(" ^ t ^ ")\n")
              else ()
           end

       fun expr () : 'a Expr.t =
           (log "expr";
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

              | Token.Match pos =>
                (adv ()
                ; let val e1 = expr ()
                  in case peek () of
                         Token.With _ => (adv ()
                                     ; Expr.Match (pos, e1, clauses ()))
                       | t => expected "with" t
                  end)

              | _ => infexp 0)

       and clauses () : (AST.Pattern.Complex.t * 'a Expr.t) list =
           (log "clauses";
            let
               val pat = pattern ()
            in
               (case peek () of
                    Token.DArrow _ => (adv (); (pat, expr ()) :: clauses' ())
                  | t => expected "=>" t)
            end)

       and clauses' () : (AST.Pattern.Complex.t * 'a Expr.t) list =
           (log "clauses'"
           ; if has ()
                then case peek () of
                         Token.Bar _ => (adv () ; clauses ())
                       | _ => []
             else [])

       and pattern () : AST.Pattern.Complex.t =
           (log "pattern"
           ; case peek () of
                 Token.Id (_, x) => (adv (); AST.Pattern.Complex.Var x)
               | Token.LParen _ => (adv ()
                               ; case peek () of
                                     Token.Ctor (_, c) => (adv ()
                                                      ; let val ctor = AST.Pattern.Complex.Ctor (c, pattern' ())
                                                        in case peek () of
                                                               Token.RParen _ => (adv (); ctor)
                                                             | t => expected "closing paren in pattern" t
                                                        end)
                                 | t => expected "ctor application in pattern" t)
               | t => expected "var or parenthesized ctor application in pattern" t)

       and pattern' () : AST.Pattern.Complex.t list =
           (log "pattern'"
           ; if has ()
                then case peek () of
                         Token.Id _ => (pattern () :: pattern' ())
                       | Token.LParen _ => (pattern () :: pattern' ())
                       | _ => []
             else [])

       and infexp (prec : int) : 'a Expr.t =
           (log "infexp";
            let
               val lhs = appexp ()
            in
               if has () andalso isBinop (peek ())
                  then infexp' (prec, lhs)
               else lhs
            end)

       and infexp' (prec : int, lhs : 'a Expr.t) : 'a Expr.t =
           (log "infexp'";
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

       and atexp () : 'a Expr.t =
           (log "atexp";
            case peek () of
                Token.Let pos =>
                (adv ()
                ; case peek () of
                      Token.Val _ =>
                      (adv ()
                      ; case peek () of
                            Token.Id (_, x) =>
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
                                                   Token.End _ => (adv (); Expr.Let (pos, x, bound, body))
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
              | Token.LParen _ => (adv (); let val ast = expr ()
                                       in case peek () of
                                              Token.RParen _ => (adv (); ast)
                                            | t => expected ")" t
                                       end)
              | t => expected "let, id or constant" t)

       (*
        * lhs is the left hand side of the (potential) application
        *)
       and appexp' (lhs : 'a Expr.t) : 'a Expr.t =
           (log "appexp'";
            if has () andalso FIRSTatexp (peek ())
               then appexp' (Expr.App (Expr.getInfo lhs, lhs, atexp ()))
            else lhs)

       and appexp () : 'a Expr.t =
           (log "appexp";
            appexp' (atexp ()))

       and ctor () : string * 'a AST.Type.t option =
           (log "ctor";
            case peek () of
                Token.Ctor (_, name) => (adv ();
                                         case peek () of
                                             Token.Of _ => (adv ();
                                                            let val (typ, rest') = parseType (!rest)
                                                            in rest := rest' ; (name, SOME typ)
                                                            end)
                                           | _ => (name, NONE))
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

       and decl () : 'a AST.Decl.t =
           (log "decl";
            case peek () of
                Token.Datatype pos => (adv (); (* TODO: parse type var declarations, i.e. datatype >'a< tree... and datatype >('a, 'b)< either... *)
                                       case peek () of
                                           Token.Id (_, id) => (adv (); case peek () of
                                                                            Token.Eqls _ => (adv (); AST.Decl.Data (pos, id, ctors ()))
                                                                          | t => expected "= in datatype decl" t)
                                         | t => expected "ident in datatype decl" t)
              | Token.Val pos => (adv ();
                                  case peek () of
                                      Token.Id (_, id) => (adv (); case peek () of
                                                                       Token.Eqls _ => (adv (); AST.Decl.Val (pos, id, expr ()))
                                                                     | t => expected "= in val decl" t)
                                    | t => expected "ident in val decl" t)
              | t => expected "datatype or val in top-level decl" t)

    in
       decl ()
    end

end
