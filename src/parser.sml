(* grammar:

 atexp -> constant | valid | let val id = <exp> in <exp> end | ( <exp> )
 appexp -> atexp appexp'
 appexp' -> e | atexp appexp'
 exp -> infixexp | if <exp> then <exp> else <exp> | fn <exp> | case <exp> of <exp>

 *   exprs  -> atexpr atexpr
 *   exprs  -> expr
 *   exprs' -> expr exprs'
 *   exprs' ->
 *   expr   -> if exprs then exprs else exprs
 *   expr   -> fn id => exprs
 *   expr   -> let id = exprs in exprs
 *   expr   -> match exprs with clause clauses
 *   clauses -> pattern => exprs clauses'
 *   clauses' -> | clauses
 *   clauses' ->
 *   pattern -> id
 *   pattern -> (ctor pattern')
 *   pattern' -> pattern pattern'
 *   pattern' ->
 *   expr   -> term expr'
 *   expr'  -> + term expr'
 *   expr'  -> - term expr'
 *   expr'  ->
 *   term   -> factor term'
 *   term'  -> * factor term'
 *   term'  -> / factor term'
 *   term'  ->
 *   factor -> ( exprs )
 *   factor -> id
 *   factor -> num
 *   factor -> bool
 *)
structure Parser : sig 

val parse : 'a Lexer.t list -> 'a Abstract.t

end =
struct

structure L = Lexer
structure A = Abstract
exception SyntaxError of string

(* check if token is in FIRST(atexp) *)
fun FIRSTatexp (L.Id (pos, _))   = true
  | FIRSTatexp (L.Num (pos, _))  = true
  | FIRSTatexp (L.Bool (pos, _)) = true
  | FIRSTatexp (L.Let pos)       = true
  | FIRSTatexp (L.LParen pos)    = true
  | FIRSTatexp _                 = false

fun isBinop (L.Add _) = true
  | isBinop (L.Sub _) = true
  | isBinop (L.Mul _) = true
  | isBinop (L.Div _) = true
  | isBinop _ = false

fun getBinop (L.Add _) = A.Add
  | getBinop (L.Sub _) = A.Sub
  | getBinop (L.Mul _) = A.Mul
  | getBinop (L.Div _) = A.Div

(*
 * Given list of tokens, return a parse tree, 'a will be some kind of position record, but the parser doesn't really care about the concrete type
 *)
fun parse (toks : 'a L.t list) : 'a A.t =
    let
       val rest = ref toks
       fun has () = not (null (!rest))
       fun adv () = rest := tl (!rest)
       fun next () = hd (!rest) before adv ()
       fun getNext () = if has () then SOME (next ()) else NONE
       fun peek () = hd (!rest)
       fun err s = raise SyntaxError ("err " ^ s)
       fun expected s t = raise SyntaxError ("expected " ^ s ^ ", got " ^ L.show t)

       (* flip this to print the grammar productions at each step *)
       val debug = true
       fun log s =
           let val t = if has () then L.show (peek ()) else ".."
           in if debug
                 then print (s ^ "(" ^ t ^ ")\n")
              else ()
           end

       fun expr () : 'a A.t =
           (log "expr";
            case peek () of
                L.If pos =>
                (adv ()
                ; let val e1 = expr ()
                  in case peek () of
                         L.Then _ => (adv ()
                                     ; let val e2 = expr ()
                                       in case peek () of
                                              L.Else _ => (adv ()
                                                          ; A.If (pos, e1, e2, expr ()))
                                            | t => expected "else" t
                                       end)
                       | t => expected "then" t
                  end)
              | L.Fn pos =>
                (adv ()
                ; case peek () of
                      L.Id (pos', x) => (adv ()
                                     ; case peek () of
                                           L.Arrow _ => (adv ()
                                                        (* FIXME: two ids for Fn *)
                                                        ; A.Fn (pos', pos, x, expr ()))
                                         | t => expected "=>" t)
                    | t => err ("expected formal arg in fn expr, got " ^ L.show t))

              | L.Match pos =>
                (adv ()
                ; let val e1 = expr ()
                  in case peek () of
                         L.With _ => (adv ()
                                     ; A.Match (pos, e1, clauses ()))
                       | t => expected "with" t
                  end)

              | _ => infexp ())

       and clauses () : (Pattern.Complex.t * 'a A.t) list =
           (log "clauses";
            let
               val pat = pattern ()
            in
               (case peek () of
                    L.Arrow _ => (adv (); (pat, expr ()) :: clauses' ())
                  | t => expected "=>" t)
            end)

       and clauses' () : (Pattern.Complex.t * 'a A.t) list =
           (log "clauses'"
           ; if has ()
                then case peek () of
                         L.Bar _ => (adv () ; clauses ())
                       | _ => []
             else [])

       and pattern () : Pattern.Complex.t =
           (log "pattern"
           ; case peek () of
                 L.Id (_, x) => (adv (); Pattern.Complex.Var x)
               | L.LParen _ => (adv ()
                               ; case peek () of
                                     L.Ctor (_, c) => (adv ()
                                                      ; let val ctor = Pattern.Complex.Ctor (c, pattern' ())
                                                        in case peek () of
                                                               L.RParen _ => (adv (); ctor)
                                                             | t => expected "closing paren in pattern" t
                                                        end)
                                 | t => expected "ctor application in pattern" t)
               | t => expected "var or parenthesized ctor application in pattern" t)

       and pattern' () : Pattern.Complex.t list =
           (log "pattern'"
           ; if has ()
                then case peek () of
                         L.Id _ => (pattern () :: pattern' ())
                       | L.LParen _ => (pattern () :: pattern' ())
                       | _ => []
             else [])

       and infexp () : 'a A.t =
           (log "infexp";
            infexp' (appexp ()))

       and infexp' (lhs : 'a A.t) : 'a A.t =
           (log "infexp'";
            if has () andalso isBinop (peek ())
               then let val t = peek ()
                    in (adv (); infexp' (A.Infix (A.getInfo lhs, getBinop t, lhs, infexp ())))
                    end
            else lhs)

       and atexp () : 'a A.t =
           (log "atexp";
            case peek () of
                L.Let pos =>
                (adv ()
                ; case peek () of
                      L.Val _ =>
                      (adv ()
                      ; case peek () of
                            L.Id (_, x) =>
                            (adv ()
                            ; case peek () of
                                  L.Eqls _ =>
                                  (adv ()
                                  ; let val bound = expr ()
                                    in case peek () of
                                           L.In _ =>
                                           (adv ();
                                            let val body = expr ()
                                            in case peek () of
                                                   L.End _ => (adv (); A.Let (pos, x, bound, body))
                                                 | t => expected "end" t
                                            end)
                                         | t => expected "in" t
                                    end)
                                | t => expected "=" t)
                          | t => err ("expected bound var in let expr, got " ^ L.show t))
                    | t => expected "val" t)
              | L.Num (pos, n) => (adv (); A.Num (pos, n))
              | L.Bool (pos, b) => (adv (); A.Bool (pos, b))
              | L.Id (pos, s) => (adv (); A.Id (pos, s))
              | L.LParen _ => (adv (); let val ast = expr ()
                                       in case peek () of
                                              L.RParen _ => (adv (); ast)
                                            | t => expected ")" t
                                       end)
              | t => expected "let, id or constant" t)

       (*
        * lhs is the left hand side of the (potential) application
        *)
       and appexp' (lhs : 'a A.t) : 'a A.t =
           (log "appexp'";
            if has () andalso FIRSTatexp (peek ())
               then appexp' (A.App (A.getInfo lhs, lhs, atexp ()))
            else lhs)

       and appexp () : 'a A.t =
           (log "appexp";
            appexp' (atexp ()))

    in
       expr ()
    end

end
