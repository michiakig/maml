(* ML grammar, adapted from the Definition of Standard ML, appendix B fig 20 p63
 *)

structure Parser : sig
   val parse : 'a Token.t list -> ('a, 'a) AST.Pgm.t
   val parseDecl : 'a Token.t list -> ('a, 'a) AST.Decl.t
   val parseExpr : 'a Token.t list -> 'a AST.Expr.t
   val parseType : 'a Token.t list -> 'a AST.Type.t
   exception SyntaxError of string
end =
struct

   exception SyntaxError of string

   (* flip this to print the grammar productions at each step *)
  val debug = false

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
       * given a list of tokens, parse a single type expression, and return the unconsumed input
       *)
      fun parseType' (ts : 'a Token.t list) : 'a AST.Type.t * 'a Token.t list =
          let
             val rest = ref ts
             fun has () = not (null (!rest))
             fun peek () = case !rest of [] => NONE | t :: _ => SOME t
             fun unsafe () = hd (!rest)
             fun adv () = rest := tl (!rest)
             fun next () = peek () before adv ()

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

             fun tyseq (acc : 'a AST.Type.t list) : 'a AST.Type.t =
                 (log "tyseq";
                  case peek () of
                      SOME (Token.RParen _) => (adv ();
                                                case peek () of
                                                    SOME (Token.Id (pos, c)) => (adv (); AST.Type.Con (pos, c, rev acc))
                                                  | SOME t => expected "tycon following tyseq in type expression" t
                                                  | NONE => raise SyntaxError "unexpected EOF")
                    | SOME (Token.Comma _) => (adv (); tyseq (infexp 0 :: acc))
                    | SOME t => expected "comma or )" t
                    | NONE => raise SyntaxError "unexpected EOF")

             (*
              * parse an atomic expression -- var or parenthesized infix
              *)
             and atom () : 'a AST.Type.t =
                 (log "atom";
                  case peek () of
                      SOME (Token.TypeVar v) => (adv (); AST.Type.Var v)
                    | SOME (Token.LParen pos)  => (adv ();
                                                   let val t = infexp 0
                                                   in
                                                      case peek () of
                                                          SOME (Token.RParen _) => (adv (); AST.Type.Paren (pos, t))
                                                        | SOME (Token.Comma _) => tyseq [t]
                                                        | SOME t => expected "comma or )" t
                                                        | NONE => raise SyntaxError "unexpected EOF"
                                                   end)
                    | SOME (Token.Id (pos, c)) => (adv(); AST.Type.Con (pos, c, []))
                    | SOME t             => expected "TypeVar, LParen, or Id" t
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
                              SOME (Token.Id (pos, c)) => (adv (); infexp' (prec, AST.Type.Con (pos, c, [lhs])))
                            | SOME t =>
                              if isInfix t
                              then
                                 let val (prec', ctor, assoc) = getPrec t
                                 in
                                    if prec < prec'
                                    then let val _ = adv ();
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

fun isBinop (Token.Infix (_, "+")) = true
  | isBinop (Token.Infix (_, "-")) = true
  | isBinop (Token.Infix (_, "*")) = true
  | isBinop (Token.Infix (_, "/")) = true
  | isBinop _ = false

fun getBinop (Token.Infix (_, oper)) = oper
  | getBinop _ = raise Match

structure Expr = AST.Expr

(*
 * Given a list of tokens, parse one declaration, return it and the remaining input
 *)
fun parseExpr' (toks : 'a Token.t list) : 'a AST.Expr.t * 'a Token.t list =
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
                         Token.Infix (_, "+") => 1
                       | Token.Infix (_, "-") => 1
                       | Token.Infix (_, "*") => 2
                       | Token.Infix (_, "/") => 2
                       | _ => 0)
            else 0)

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
                         Token.Of _ => (adv ()
                                     ; Expr.Case (pos, e1, clauses ()))
                       | t => expected "of" t
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
           (log "patterns"
           ; if has ()
                then case peek () of
                         Token.Comma _ => (adv (); pattern () :: patterns ())
                       | Token.RParen _ => (adv (); [])
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

       and tuple () : 'a Expr.t list =
           (log "tuple";
            case peek () of
                Token.Comma _ => (adv (); let val e = expr ()
                                          in e :: tuple ()
                                          end)
              | Token.RParen _ => (adv (); [])
              | t => expected "comma or )" t)

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
           (log "appexp'";
            if has () andalso FIRSTatexp (peek ())
               then appexp' (Expr.App (Expr.getInfo lhs, lhs, atexp ()))
            else lhs)

       and appexp () : 'a Expr.t =
           (log "appexp";
            appexp' (atexp ()))

    in
       (expr (), !rest)
    end

fun parseDecl' (toks : 'a Token.t list) : ('a, 'a) AST.Decl.t * 'a Token.t list =
    let
       val rest = ref toks
       fun has () = not (null (!rest))
       fun adv () = rest := tl (!rest)
       fun next () = hd (!rest) before adv ()
       fun getNext () = if has () then SOME (next ()) else NONE
       fun peek () = hd (!rest)
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
                                                               let val (typ, rest') = parseType' (!rest)
                                                               in rest := rest' ; (name, SOME typ)
                                                               end)
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
                                                                                        let
                                                                                           val (e, rest') = parseExpr' (!rest)
                                                                                        in
                                                                                           rest := rest'
                                                                                         ; AST.Decl.Val (pos, id, e)
                                                                                        end)
                                                                     | t => expected "= in val decl" t)
                                    | t => expected "ident in val decl" t)
              | t => expected "datatype or val in top-level decl" t)
    in
       (decl (), !rest)
    end

fun parse (toks : 'a Token.t list) : ('a, 'a) AST.Pgm.t =
    let
       fun parse' (decl, []) = [decl]
         | parse' (decl, rest) =
           decl :: parse' (parseDecl' rest)
    in
       parse' (parseDecl' toks)
    end

fun parseExpr (toks : 'a Token.t list) : 'a AST.Expr.t =
    let
       val (ast, _) = parseExpr' toks
    in
       ast
    end

fun parseType (toks : 'a Token.t list) : 'a AST.Type.t =
    let
       val (ast, _) = parseType' toks
    in
       ast
    end

fun parseDecl (toks : 'a Token.t list) : ('a, 'a) AST.Decl.t =
    let
       val (ast, _) = parseDecl' toks
    in
       ast
    end

end
