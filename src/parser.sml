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

   fun log rdr s tag =
       let val t = case rdr s of
                       SOME ((t, _), _) => Token.show t
                     | NONE => ".."
       in
          if debug then
             print (tag ^ "(" ^ t ^ ")\n")
          else ()
       end

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

       fun adv () = rest := (case rdr (!rest) of
                                 NONE => raise Empty
                               | SOME (_, s) => s)

       fun getPos () =
           case rdr (!rest) of
               SOME ((_, p), _) => p
             | NONE             => raise CompilerBug "getPos called on empty stream"

       fun peek () =
           case rdr (!rest) of
               SOME ((t, _), _) => SOME t
             | NONE             => NONE

       fun next () =
           case peek () of
               SOME t => (adv (); t)
             | NONE => raise CompilerBug "next () called on empty stream"

       fun match t =
           case peek () of
               SOME t' => if t = t' then
                             adv ()
                          else expected rdr (!rest) (Token.show t)
             | NONE => expected rdr (!rest) (Token.show t)

       fun getPrec () : int =
           case peek () of
               SOME (Token.Infix "+") => 1
             | SOME (Token.Infix "-") => 1
             | SOME (Token.Infix "*") => 2
             | SOME (Token.Infix "/") => 2
             | _ => 0

       fun parseIf () =
           let
              val p  = getPos ()
              val _  = match Token.If
              val e1 = expr ()
              val _ = match Token.Then
              val e2 = expr ()
              val _ = match Token.Else
              val e3 = expr ()
           in
              Expr.If (p, e1, e2, e3)
           end

       (* attempt to parse an identifier. consume and return it if successful *)
       and parseId () =
           case peek () of
               SOME (Token.Id id) => (adv (); id)
             | SOME t             => raise SyntaxError ("expected Id, but got " ^ Token.show t)
             | NONE               =>  "eof"

       and parseFn () =
           let
              val p  = getPos ()
              val _  = match Token.Fn
              val p' = getPos ()
              val x  = parseId ()
              val _  = match Token.DArrow
              val b  = expr ()
           in
              Expr.Fn (p', p, x, b)
           end

       and parseCase () =
           let
              val p  = getPos ()
              val _  = match Token.Case
              val e  = expr ()
              val _  = match Token.Of
              val cs = clauses ()
           in
              Expr.Case (p, e, cs)
           end

       and expr () : Pos.t Expr.t =
           (log rdr (!rest) "expr";
            case peek () of
                SOME Token.If   => parseIf ()
              | SOME Token.Fn   => parseFn ()
              | SOME Token.Case => parseCase ()
              | _ => infexp 0)

       and clauses () : (AST.Pattern.Complex.t * Pos.t Expr.t) list =
           (log rdr (!rest) "clauses";
            let
               val pat = pattern ()
               val _ = match Token.DArrow
            in
               (pat, expr ()) :: clauses' ()
            end)

       and clauses' () : (AST.Pattern.Complex.t * Pos.t Expr.t) list =
           (log rdr (!rest) "clauses'"
           ; case peek () of
                 SOME Token.Bar => (adv () ; clauses ())
               | _ => [])

       and pattern () : AST.Pattern.Complex.t =
           (log rdr (!rest) "pattern"
           ; case peek () of
                 SOME (Token.Id x) => (adv (); AST.Pattern.Complex.Var x)
               | SOME (Token.Ctor c) => (adv ();
                                         case peek () of
                                                   SOME (Token.Id _)   => AST.Pattern.Complex.Ctor (c, SOME (pattern ()))
                                                 | SOME (Token.LParen) => AST.Pattern.Complex.Ctor (c, SOME (pattern ()))
                                                 | SOME _              => AST.Pattern.Complex.Ctor (c, NONE)
                                                 | NONE                => AST.Pattern.Complex.Ctor (c, NONE))
               | SOME Token.LParen => (adv ()
                                      ; let val p = pattern ()
                                        in
                                           case peek () of
                                               SOME Token.Comma => AST.Pattern.Complex.Tuple (p :: patterns ())
                                             | SOME Token.RParen => (adv (); p)
                                             | _ => expected rdr (!rest) "comma or ) in pattern"
                                        end)
               | _ => expected rdr (!rest) "var, tuple, or ctor application in pattern")

       and patterns () : AST.Pattern.Complex.t list =
           (log rdr (!rest) "patterns"
           ; case peek () of
                 SOME Token.Comma => (adv (); pattern () :: patterns ())
               | SOME Token.RParen => (adv (); [])
               | _ => [])

       and infexp (prec : int) : Pos.t Expr.t =
           (log rdr (!rest) "infexp";
            let
               val lhs = appexp ()
            in
               case peek () of
                   SOME t => if isBinop t then
                                infexp' (prec, lhs)
                             else lhs
                | NONE => lhs
            end)

       and infexp' (prec : int, lhs : Pos.t Expr.t) : Pos.t Expr.t =
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

       and tuple () : Pos.t Expr.t list =
           (log rdr (!rest) "tuple";
            case peek () of
                SOME Token.Comma => (adv (); let val e = expr ()
                                             in e :: tuple ()
                                             end)
              | SOME Token.RParen => (adv (); [])
              | _ => expected rdr (!rest) "comma or )")

       and parseLet () =
           let
              val p  = getPos ()
              val _  = match Token.Let
              val _  = match Token.Val
              val p' = getPos ()
              val x  = parseId ()
              val _  = match Token.Eqls
              val e  = expr ()
              val _  = match Token.In
              val b  = expr ()
              val _  = match Token.End
           in
              Expr.Let (p', p, x, e, b)
           end

       and atexp () : Pos.t Expr.t =
           (log rdr (!rest) "atexp";
            case rdr (!rest) of
                SOME ((Token.Let, _), _)    => parseLet ()
              | SOME ((Token.Num  n, p), _) => (adv (); Expr.Num  (p, n))
              | SOME ((Token.Bool b, p), _) => (adv (); Expr.Bool (p, b))
              | SOME ((Token.Id   x, p), _) => (adv (); Expr.Id   (p, x))

              (* in the context of an atexp, parse a Ctor as an Ident. *)
              | SOME ((Token.Ctor c, p), _) => (adv (); Expr.Id (p, c))

              | SOME ((Token.LParen, p), _) =>
                (adv ();
                 let
                    val e = expr ()
                 in
                    case peek () of
                        SOME Token.RParen => (adv (); e)
                      | SOME Token.Comma  => Expr.Tuple (p, e :: tuple ())
                      | _ => expected rdr (!rest) "comma or )"
                 end)
              | _ => expected rdr (!rest) "let, id or constant")

       (*
        * lhs is the left hand side of the (potential) application
        *)
       and appexp' (lhs : Pos.t Expr.t) : Pos.t Expr.t =
           (log rdr (!rest) "appexp'";
            case peek () of
                SOME t => if FIRSTatexp t then
                             appexp' (Expr.App (Expr.getInfo lhs, lhs, atexp ()))
                          else lhs
              | NONE => lhs)

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

       (* skip the current token and advance the pointer to the stream *)
       fun adv () = rest := (case rdr (!rest) of
                                 NONE => raise CompilerBug "trying to advance past EOF in decl parser"
                               | SOME (_, s) => s)

       (* look ahead one token *)
       fun peek () =
           case rdr (!rest) of
               SOME ((t, _), _) => SOME t
             | NONE             => NONE

       (* extract the position from the token at the head of the stream *)
       fun getPos () =
           case rdr (!rest) of
               SOME ((_, p), _) => p
             | NONE             => raise CompilerBug "getPos called on empty stream"

       (* look ahead one token and compare it to the given token, advancing if they match *)
       fun match t =
           case peek () of
               SOME t' => if t = t' then
                             adv ()
                          else expected rdr (!rest) (Token.show t)
             | NONE    => expected rdr (!rest) (Token.show t)


       (* parse a single constructor, returning its name and (optionally) its argument type *)
       fun ctor () : string * Pos.t AST.Type.t option =
           (log rdr (!rest) "ctor";
            let
               val c = case peek () of
                           SOME (Token.Ctor c) => (adv (); c)
                         | _                   => expected rdr (!rest) "constructor"
               val t = case peek () of
                           SOME Token.Of =>
                           ( adv ()
                           ; case makeType rdr (!rest) of
                                 SOME (t, rest') => (rest := rest' ; SOME t)
                               | NONE            => expected rdr (!rest) "type after `Of` in datatype constructor")
                         | _ => NONE
            in
               (c, t)
            end)

       (* parse one or more constructors, separated by bars *)
       and ctors () : (string * Pos.t AST.Type.t option) list =
           let
              fun ctors' () : (string * Pos.t AST.Type.t option) list =
                  case peek () of
                      SOME Token.Bar => (adv (); ctor () :: ctors' ())
                    | _ => []
           in
              (log rdr (!rest) "ctors"; ctor () :: ctors' ())
           end

       (* parse a comma-delimited list of type variables between parentheses *)
       and typeVars () =
           case peek () of
               SOME (Token.TypeVar t) => (adv (); [t])
             | SOME Token.LParen =>
               let
                  fun typeVars' () =
                      case peek () of
                          SOME (Token.TypeVar t) => (adv (); t :: typeVars' ())
                        | SOME Token.Comma       => (adv (); typeVars' ())
                        | _                      => []

                  val _ = match Token.LParen
                  val ts = typeVars' ()
                  val _ = match Token.RParen
               in
                  ts (* TODO this allows `datatype () foo = ...` *)
               end
             | _ => []


       (* parse a datatype declaration *)
       and data () =
           (log rdr (!rest) "data";
            let
               val p  = getPos ()
               val _  = match Token.Datatype
               val ts = typeVars ()
               val x  = id ()
               val _  = match Token.Eqls
               val cs = ctors ()
            in
               AST.Decl.Data (p, ts, x, cs)
            end)

       (* attempt to parse an identifier. consume and return it if successful *)
       and id () =
           case peek () of
               SOME (Token.Id id) => (adv (); id)
             | _                  => expected rdr (!rest) "identifier"

       and value () =
           let
              val p = getPos ()
              val _ = match Token.Val
              val x = id ()
              val _ = match Token.Eqls
           in
              case makeExpr rdr (!rest) of
                  SOME (e, rest') => (rest := rest'; AST.Decl.Val (p, x, e))
                | NONE            => expected rdr (!rest) "expression in value declaration"
           end

       and decl () : ((Pos.t, Pos.t) AST.Decl.t * 'a) option =
           (log rdr (!rest) "decl";
            case peek () of
                SOME Token.Datatype => SOME (data (), !rest)
              | SOME Token.Val      => SOME (value (), !rest)
              | SOME _              => expected rdr (!rest) "datatype or value declaration"
              | NONE                => NONE)
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
