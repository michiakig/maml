(* grammar:
 *   exprs  -> expr expr
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
       val debug = false
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
                ; let val e1 = exprs ()
                  in case peek () of
                         L.Then _ => (adv ()
                                     ; let val e2 = exprs ()
                                       in case peek () of
                                              L.Else _ => (adv ()
                                                          ; A.If (pos, e1, e2, exprs ()))
                                            | t => expected "else" t
                                       end)
                       | t => expected "then" t
                  end)
              | L.Fn pos =>
                (adv ()
                ; case peek () of
                      L.Id (_, x) => (adv ()
                                     ; case peek () of
                                           L.Arrow _ => (adv ()
                                                        ; A.Fn (pos, x, exprs ()))
                                         | t => expected "=>" t)
                    | t => err ("expected formal arg in fn expr, got " ^ L.show t))
              | L.Let pos =>
                (adv ()
                ; case peek () of
                      L.Id (_, x) => (adv ()
                                     ; case peek () of
                                           L.Eqls _ => (adv ()
                                                       ; let val bound = exprs ()
                                                         in case peek () of
                                                                L.In _ => (adv (); A.Let (pos, x, bound, exprs ()))
                                                              | t => expected "in" t
                                                         end)
                                         | t => expected "=" t)
                    | t => err ("expected bound var in let expr, got " ^ L.show t))

              | L.Match pos =>
                (adv ()
                ; let val e1 = exprs ()
                  in case peek () of
                         L.With _ => (adv ()
                                     ; A.Match (pos, e1, clauses ()))
                       | t => expected "with" t
                  end)

              | _ => expr' (term ()))

       and clauses () : (Pattern.Complex.t * 'a A.t) list =
           (log "clauses";
            let
               val pat = pattern ()
            in
               (case peek () of
                    L.Arrow _ => (adv (); (pat, exprs ()) :: clauses' ())
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

       and term () : 'a A.t =
           (log "term";
            let
               val lhs = factor ()
            in
               term' lhs
            end)

       and expr' (lhs : 'a A.t) : 'a A.t =
           (log "expr'";
           if has ()
              then case peek () of
                       L.Add pos => (next (); expr' (A.Add (pos, lhs, term ())))
                     | L.Sub pos => (next (); expr' (A.Sub (pos, lhs, term ())))
                     | _ => lhs
           else lhs)

       and term' (lhs : 'a A.t) : 'a A.t =
           (log "term'";
           if has ()
              then case peek () of
                       L.Mul pos => (next (); term' (A.Mul (pos, lhs, factor ())))
                     | L.Div pos => (next (); term' (A.Div (pos, lhs, factor ())))
                     | _ => lhs
           else lhs)

       and factor () : 'a A.t =
           (log "factor";
            case getNext () of
                SOME (L.LParen _) => let val ast = exprs ()
                                     in case getNext () of
                                            SOME (L.RParen _) => ast
                                          | SOME t => expected ")" t
                                          | _ => err "unexpected end of input, expected )"
                                     end
              | SOME (L.Num (pos, n)) => A.Num (pos, n)
              | SOME (L.Bool (pos, b)) => A.Bool (pos, b)
              | SOME (L.Id (pos, s)) => A.Id (pos, s)
              | SOME t => expected "bool, num or id" t
              | _ => err "unexpected end of input, expected bool, num or id")

       and exprs () : 'a A.t =
           let
              (* check if token is in FIRST(expr) *)
              fun FIRSTexpr (L.Id (pos, _))   = SOME pos
                | FIRSTexpr (L.Num (pos, _))  = SOME pos
                | FIRSTexpr (L.Bool (pos, _)) = SOME pos
                | FIRSTexpr (L.If pos)        = SOME pos
                | FIRSTexpr (L.Fn pos)        = SOME pos
                | FIRSTexpr (L.Let pos)       = SOME pos
                | FIRSTexpr (L.LParen pos)    = SOME pos
                | FIRSTexpr _                 = NONE

              val ast1 = expr ()
           in
              if has ()
                 then case FIRSTexpr (peek ()) of
                          SOME pos => A.App (pos, ast1, expr ())
                        | NONE => ast1
              else ast1
           end
    in
       exprs ()
    end

end
