(* Parser for a restricted subset of the ML grammar, adapted from the
 * Definition of Standard ML, appendix B fig 20 p63 *)
structure Parser : sig
   val parse    : (Token.t * Pos.t, 'a) Reader.t -> 'a -> (Pos.t, Pos.t) AST.Pgm.t
   val makeDecl : (Token.t * Pos.t, 'a) Reader.t -> ((Pos.t, Pos.t) AST.Decl.t, 'a) Reader.t
   val makeExpr : (Token.t * Pos.t, 'a) Reader.t -> (Pos.t AST.Expr.t, 'a) Reader.t
   val makeType : (Token.t * Pos.t, 'a) Reader.t -> (Pos.t AST.Type.t, 'a) Reader.t
   exception SyntaxError of string
end =
struct
   open Top

   exception SyntaxError of string

   (* flip this to print the grammar productions at each step *)
   val debug = false

   fun log rdr s msg =
       if debug then
          print (msg ^ "(" ^
                 (case rdr s of
                      NONE => ".."
                    | SOME ((t, _), _) => Token.show t)
                 ^ ")\n")
       else ()

   fun expected rdr s msg =
       let
          val got = case rdr s of
                        NONE => "unexpected EOF"
                      | SOME ((t, _), _) => Token.show t
       in
          raise SyntaxError ("expected " ^ msg ^ ", got " ^ got)
       end

   (*
    * Pratt parser for type expressions
    *)
   local

      datatype assoc = Left | Right

      exception NoPrecedence of string
      fun getPrec (Token.Infix "*") = (60, fn (pos, AST.Type.Tuple (_, xs), y) => AST.Type.Tuple (pos, xs @ [y]) | (pos, x, y) => AST.Type.Tuple (pos, [x, y]), Left)
        | getPrec Token.TArrow      = (50, AST.Type.Arrow, Right)
        | getPrec t                 = raise NoPrecedence (Token.show t)

      fun isInfix (Token.Infix "*") = true
        | isInfix Token.TArrow      = true
        | isInfix _                 = false

   in

      (*
       * accepts a token reader and returns a reader for type expressions
       *)
      fun makeType (rdr : (Token.t * Pos.t, 'b) Reader.t) : (Pos.t AST.Type.t, 'b) Reader.t =
         fn s =>
          let
             (*
              * parse a type sequence as argument to a type ctor, i.e. ('a, 'b) either
              *)
             fun tyseq s acc =
                 (log rdr s "tyseq";
                  case rdr s of
                      SOME ((Token.RParen, _), s') => (case rdr s' of
                                                        SOME ((Token.Id c, p), s'') => SOME (AST.Type.Con (p, c, rev acc), s'')
                                                      |  _                          => expected rdr s' "tycon following tyseq in type expression")
                    | SOME ((Token.Comma, _), s')  => (case infexp s' 0 of
                                                       SOME (t, s'') => tyseq s'' (t :: acc)
                                                     | NONE          => expected rdr s' "type expression following comma in tyseq")
                    | _ => expected rdr s "comma or right paren")

             (*
              * parse an atomic expression -- var or parenthesized infix
              *)
             and atom s =
                 (log rdr s "atom";
                  case rdr s of
                      SOME ((Token.TypeVar v, p), s') => SOME (AST.Type.Var (p, v), s')
                    | SOME ((Token.LParen, p), s') =>
                      (case infexp s' 0 of
                           SOME (ty, s'') =>
                           (case rdr s'' of
                                SOME ((Token.RParen, _), s''') => SOME (AST.Type.Paren (p, ty), s''')
                              | SOME ((Token.Comma,  _), s''') => tyseq s'' [ty]
                              | _                              => expected rdr s'' "comma or right paren")
                         | NONE => expected rdr s' "type expression after left paren")
                    | SOME ((Token.Id c, p), s') => SOME (AST.Type.Con (p, c, []), s')
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
                              SOME ((Token.Id c, p), s') => (infexp' s' prec (AST.Type.Con (p, c, [lhs])))
                            | SOME ((t, p), s') =>
                              if isInfix t then
                                 let val (prec', ctor, assoc) = getPrec t
                                 in
                                    if prec < prec' then
                                       let
                                          val prec'' = case assoc of Left => prec' | Right => prec' - 1
                                       in
                                          case infexp s' prec'' of
                                              SOME (ty, s'') => infexp' s'' prec (ctor (p, lhs, ty))
                                            | _              => expected rdr s' "right hand side in type expression"
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
fun FIRSTatexp (Token.Id   _) = true
  | FIRSTatexp (Token.Num  _) = true
  | FIRSTatexp (Token.Bool _) = true
  | FIRSTatexp  Token.Let     = true
  | FIRSTatexp  Token.LParen  = true
  | FIRSTatexp  _             = false

fun isBinop (Token.Infix "+") = true
  | isBinop (Token.Infix "-") = true
  | isBinop (Token.Infix "*") = true
  | isBinop (Token.Infix "/") = true
  | isBinop  _                = false

fun getBinop (Token.Infix oper) = oper
  | getBinop  _                 = raise Match

structure Expr = AST.Expr

(*
 * accepts a token reader and returns a reader for expressions
 *)
fun makeExpr (rdr : (Token.t * Pos.t, 'a) Reader.t) : (Pos.t AST.Expr.t, 'a) Reader.t =
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
                         (Token.Infix "+", _) => 1
                       | (Token.Infix "-", _) => 1
                       | (Token.Infix "*", _) => 2
                       | (Token.Infix "/", _) => 2
                       | _ => 0)
            else 0)

       fun expr () : Pos.t Expr.t =
           (log rdr (!rest) "expr";
            case peek () of
                (Token.If, pos) =>
                (adv ()
                ; let val e1 = expr ()
                  in case peek () of
                         (Token.Then, _) => (adv ()
                                     ; let val e2 = expr ()
                                       in case peek () of
                                              (Token.Else, _) => (adv ()
                                                          ; Expr.If (pos, e1, e2, expr ()))
                                            | (t, _) => expected "else" t
                                       end)
                       | (t, _) => expected "then" t
                  end)
              | (Token.Fn, pos) =>
                (adv ()
                ; case peek () of
                      (Token.Id x, pos') => (adv ()
                                     ; case peek () of
                                           (Token.DArrow, _) => (adv ()
                                                        (* FIXME: two ids for Fn *)
                                                        ; Expr.Fn (pos', pos, x, expr ()))
                                         | (t, _) => expected "=>" t)
                    | (t, _) => err ("expected formal arg in fn expr, got " ^ Token.show t))

              | (Token.Case, pos) =>
                (adv ()
                ; let val e1 = expr ()
                  in case peek () of
                         (Token.Of, _) => (adv ()
                                     ; Expr.Case (pos, e1, clauses ()))
                       | (t, _) => expected "of" t
                  end)

              | _ => infexp 0)

       and clauses () : (AST.Pattern.Complex.t * Pos.t Expr.t) list =
           (log rdr (!rest) "clauses";
            let
               val pat = pattern ()
            in
               (case peek () of
                    (Token.DArrow, _) => (adv (); (pat, expr ()) :: clauses' ())
                  | (t, _) => expected "=>" t)
            end)

       and clauses' () : (AST.Pattern.Complex.t * Pos.t Expr.t) list =
           (log rdr (!rest) "clauses'"
           ; if has ()
                then case peek () of
                         (Token.Bar, _) => (adv () ; clauses ())
                       | _ => []
             else [])

       and pattern () : AST.Pattern.Complex.t =
           (log rdr (!rest) "pattern"
           ; case peek () of
                 (Token.Id   x, _) => (adv (); AST.Pattern.Complex.Var x)
               | (Token.Ctor c, _) => (adv ();
                                       if has ()
                                          then case peek () of
                                                   (Token.Id _,   _) => AST.Pattern.Complex.Ctor (c, SOME (pattern ()))
                                                 | (Token.LParen, _) => AST.Pattern.Complex.Ctor (c, SOME (pattern ()))
                                                 |  _                => AST.Pattern.Complex.Ctor (c, NONE)
                                       else AST.Pattern.Complex.Ctor (c, NONE))
               | (Token.LParen, _) => (adv ()
                                   ; let val p = pattern ()
                                     in
                                        case peek () of
                                            (Token.Comma,  _) => AST.Pattern.Complex.Tuple (p :: patterns ())
                                          | (Token.RParen, _) => (adv (); p)
                                          | (t, _) => expected "comma or ) in pattern" t
                                     end)
               | (t, _) => expected "var, tuple, or ctor application in pattern" t)

       and patterns () : AST.Pattern.Complex.t list =
           (log rdr (!rest) "patterns"
           ; if has ()
                then case peek () of
                         (Token.Comma, _)  => (adv (); pattern () :: patterns ())
                       | (Token.RParen, _) => (adv (); [])
                       | _ => []
             else [])

       and infexp (prec : int) : Pos.t Expr.t =
           (log rdr (!rest) "infexp";
            let
               val lhs = appexp ()
            in
               if has () then
                  let
                     val (t, _) = peek ()
                  in
                     if isBinop t then
                        infexp' (prec, lhs)
                     else lhs
                  end
               else lhs
            end)

       and infexp' (prec : int, lhs : Pos.t Expr.t) : Pos.t Expr.t =
           (log rdr (!rest) "infexp'";
            let
               val prec' = getPrec ()
            in
               if prec < prec'
                  then let val (t, _) = next ()
                           (* TODO: check if t is a binop *)
                           val lhs = Expr.Infix (Expr.getInfo lhs, getBinop t, lhs,
                                              infexp prec')
                       in infexp' (prec, lhs)
                       end
               else lhs
            end)

       and tuple () : Pos.t Expr.t list =
           (log rdr (!rest) "tuple";
            case peek () of
                (Token.Comma, _) => (adv (); let val e = expr ()
                                          in e :: tuple ()
                                          end)
              | (Token.RParen, _) => (adv (); [])
              | (t, _) => expected "comma or )" t)

       and atexp () : Pos.t Expr.t =
           (log rdr (!rest) "atexp";
            case peek () of
                (Token.Let, pos) =>
                (adv ()
                ; case peek () of
                      (Token.Val, _) =>
                      (adv ()
                      ; case peek () of
                            (Token.Id x, pos') =>
                            (adv ()
                            ; case peek () of
                                  (Token.Eqls, _) =>
                                  (adv ()
                                  ; let val bound = expr ()
                                    in case peek () of
                                           (Token.In, _) =>
                                           (adv ();
                                            let val body = expr ()
                                            in case peek () of
                                                   (Token.End, _) => (adv (); Expr.Let (pos', pos, x, bound, body))
                                                 | (t, _) => expected "end" t
                                            end)
                                         | (t, _) => expected "in" t
                                    end)
                                | (t, _) => expected "=" t)
                          | (t, _) => err ("expected bound var in let expr, got " ^ Token.show t))
                    | (t, _) => expected "val" t)
              | (Token.Num n, pos) => (adv (); Expr.Num (pos, n))
              | (Token.Bool b, pos) => (adv (); Expr.Bool (pos, b))
              | (Token.Id s, pos) => (adv (); Expr.Id (pos, s))

              (* in the context of an atexp, parse a Ctor as an Ident. *)
              | (Token.Ctor s, pos) => (adv (); Expr.Id (pos, s))

              | (Token.LParen, pos) =>
                (adv ();
                 let
                    val e = expr ()
                 in
                    case peek () of
                        (Token.RParen, _) => (adv (); e)
                      | (Token.Comma, _) => Expr.Tuple (pos, e :: tuple ())
                      | (t, _) => expected "comma or )" t
                 end)
              | (t, _) => expected "let, id or constant" t)

       (*
        * lhs is the left hand side of the (potential) application
        *)
       and appexp' (lhs : Pos.t Expr.t) : Pos.t Expr.t =
           (log rdr (!rest) "appexp'";
            if has () then
               let
                  val (t, _) = peek ()
               in
                  if FIRSTatexp t then
                     appexp' (Expr.App (Expr.getInfo lhs, lhs, atexp ()))
                  else lhs
               end
            else lhs)

       and appexp () : Pos.t Expr.t =
           (log rdr (!rest) "appexp";
            appexp' (atexp ()))

    in
       SOME (expr (), !rest)
    end

(*
 * accepts a token reader and returns a reader for declarations
 *)
fun makeDecl (rdr : (Token.t * Pos.t, 'a) Reader.t) : ((Pos.t, Pos.t) AST.Decl.t, 'a) Reader.t =
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
                         SOME (t, _) => SOME t
                       | NONE        => NONE
       fun peek' () =
           case rdr (!rest) of
               SOME ((t, _), _) => SOME t
             | NONE             => NONE

       fun next () = peek () before adv ()

       fun getNext () = if has () then SOME (next ()) else NONE

       fun error msg = expected rdr (!rest) msg

       fun getPos () =
           case rdr (!rest) of
               SOME ((_, p), _) => p
             | NONE             => raise CompilerBug "getPos called on empty stream"

       fun match t =
           case peek' () of
               SOME t' => if t = t' then
                             adv ()
                          else error (Token.show t')
             | NONE    => error "eof"

       fun log s =
           let
              val t = case peek () of
                          SOME (t, _) => Token.show t
                        | NONE        => ".."
           in if debug
                 then print (s ^ "(" ^ t ^ ")\n")
              else ()
           end

       fun ctor () : string * Pos.t AST.Type.t option =
           (log "ctor";
            case peek () of
                SOME (Token.Ctor name, _) =>
                (adv ();
                 case peek () of
                     SOME (Token.Of, _) =>
                     (adv ();
                      case (makeType rdr) (!rest) of
                          NONE => raise Empty
                        | SOME (typ, rest') => (rest := rest' ; (name, SOME typ)))
                   | _ => (name, NONE))
              | _ => error "ctor in datatype decl")

       and ctors' () : (string * Pos.t AST.Type.t option) list =
           (log "ctors'";
            case peek () of
                SOME (Token.Bar, _) => (adv (); ctor () :: ctors' ())
              | _ => [])

       and ctors () : (string * Pos.t AST.Type.t option) list =
           (log "ctors";
            ctor () :: ctors' ())

       (* parse a datatype declaration *)
       and data pos : (Pos.t, Pos.t) AST.Decl.t =
           let
              (* parse the rest of a datatype declaration, after `datatype 'a` or `datatype ('a, 'a)` *)
              fun data' tyvars =
                  case peek () of
                      SOME (Token.Id id, _) =>
                      (adv ();
                       case peek () of
                           SOME (Token.Eqls, _) => (adv (); AST.Decl.Data (pos, tyvars, id, ctors ()))
                         | _ => error "= in datatype decl")
                    | _ => error "identifier in datatype declaration"

              (* parse a list of type vars (after open paren): 'a, 'b, ...)` *)
              fun tyvars () =
                  case peek () of
                      SOME (Token.Comma, _)  => (adv (); case peek () of
                                                             SOME (Token.TypeVar tyvar, _) => (adv (); tyvar :: tyvars ())
                                                           | _ => error "type variable in datatype declaration")
                    | SOME (Token.RParen, _) => (adv (); [])
                    | _ => error "comma or ) in datatype declaration"
           in
              log "data"
            ; case peek () of
                  SOME (Token.TypeVar tyvar, _) => (adv (); data' [tyvar])
                | SOME (Token.LParen, _) =>
                  (adv (); case peek () of
                               SOME (Token.TypeVar tyvar, _) => (adv (); data' (tyvar :: tyvars ()))
                             | _ => error "type variable in datatype declaration")
                | _ => data' []
           end

       (* attempt to parse an identifier. consume and return it if successful *)
       and id () =
           case peek' () of
               SOME (Token.Id id) => (adv (); id)
             | SOME t             => raise SyntaxError ("expected Id, but got " ^ Token.show t)
             | NONE               => error "eof"

       and value () =
           let
              val p = getPos ()
              val _ = match Token.Val
              val x = id ()
              val _ = match Token.Eqls
           in
              case makeExpr rdr (!rest) of
                  SOME (e, rest') => (rest := rest'; SOME (AST.Decl.Val (p, x, e), !rest))
                | NONE            => error "expression in val decl"
           end

       and decl () : ((Pos.t, Pos.t) AST.Decl.t * 'a) option =
           (log "decl";
            case peek () of
                SOME (Token.Datatype, pos) => (adv (); SOME (data pos, !rest))
              | SOME (Token.Val, _) => value ()
              | SOME _ => error "`datatype` or `val` in top-level decl"
              | NONE => NONE)
    in
       decl ()
    end

fun parse (rdr : (Token.t * Pos.t, 'a) Reader.t) (s : 'a) : (Pos.t, Pos.t) AST.Pgm.t =
    let
       val decl = makeDecl rdr

       fun parse' s =
           case decl s of
               NONE => []
             | SOME (ast, s') => ast :: parse' s'
    in
       parse' s
    end

end
