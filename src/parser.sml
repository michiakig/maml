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

val parse : Lexer.t list -> Abstract.t

end =
struct

structure L = Lexer
structure A = Abstract
exception SyntaxError of string

fun parse toks =
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

       fun expr () : A.t =
           (log "expr";
            case peek () of
                L.If =>
                (adv ()
                ; let val e1 = exprs ()
                  in case peek () of
                         L.Then => (adv ()
                                   ; let val e2 = exprs ()
                                     in case peek () of
                                            L.Else => (adv ()
                                                      ; A.If (e1, e2, exprs ()))
                                          | t => expected "else" t
                                     end)
                       | t => expected "then" t
                  end)
              | L.Fn =>
                (adv ()
                ; case peek () of
                      L.Id x => (adv ()
                                ; case peek () of
                                      L.Arrow => (adv (); A.Fn (x, exprs ()))
                                    | t => expected "=>" t)
                    | t => err ("expected formal arg in fn expr, got " ^ L.show t))
              | L.Let =>
                (adv ()
                ; case peek () of
                      L.Id x => (adv ()
                                ; case peek () of
                                      L.Eqls => (adv ()
                                              ; let val bound = exprs ()
                                                in case peek () of
                                                       L.In => (adv (); A.Let (x, bound, exprs ()))
                                                     | t => expected "in" t
                                                end)
                                    | t => expected "=" t)
                    | t => err ("expected bound var in let expr, got " ^ L.show t))

              | L.Match =>
                (adv ()
                ; let val e1 = exprs ()
                  in case peek () of
                         L.With => (adv ()
                                 ; A.Match (e1, clauses ()))
                       | t => expected "with" t
                  end)

              | _ => expr' (term ()))

       and clauses () : (Pattern.Complex.t * A.t) list =
           (log "clauses";
            let
               val pat = pattern ()
            in
               (case peek () of
                    L.Arrow => (adv (); (pat, exprs ()) :: clauses' ())
                  | t => expected "=>" t)
            end)

       and clauses' () : (Pattern.Complex.t * A.t) list =
           (log "clauses'"
           ; if has ()
                then case peek () of
                         L.Bar => (adv () ; clauses ())
                       | _ => []
             else [])

       and pattern () : Pattern.Complex.t =
           (log "pattern"
           ; case peek () of
                 L.Id x => (adv (); Pattern.Complex.Var x)
               | L.LParen => (adv ()
                             ; case peek () of
                                   L.Ctor c => (adv ()
                                               ; let val ctor = Pattern.Complex.Ctor (c, pattern' ())
                                                 in case peek () of
                                                        L.RParen => (adv (); ctor)
                                                      | t => expected "closing paren in pattern" t
                                                 end)
                                 | t => expected "ctor application in pattern" t)
               | t => expected "var or parenthesized ctor application in pattern" t)

       and pattern' () : Pattern.Complex.t list =
           (log "pattern'"
           ; if has ()
                then case peek () of
                         L.Id _ => (pattern () :: pattern' ())
                       | L.LParen => (pattern () :: pattern' ())
                       | _ => []
             else [])

       and term () : A.t =
           (log "term";
            let
               val lhs = factor ()
            in
               term' lhs
            end)

       and expr' (lhs : A.t) : A.t =
           (log "expr'";
           if has ()
              then case peek () of
                       L.Add => (next (); expr' (A.Add (lhs, term ())))
                     | L.Sub => (next (); expr' (A.Sub (lhs, term ())))
                     | _ => lhs
           else lhs)

       and term' (lhs : A.t) : A.t =
           (log "term'";
           if has ()
              then case peek () of
                       L.Mul => (next (); term' (A.Mul (lhs, factor ())))
                     | L.Div => (next (); term' (A.Div (lhs, factor ())))
                     | _ => lhs
           else lhs)

       and factor () : A.t =
           (log "factor";
            case getNext () of
                SOME L.LParen => let val ast = exprs ()
                                 in case getNext () of
                                        SOME L.RParen => ast
                                      | SOME t => expected ")" t
                                      | _ => err "unexpected end of input, expected )"
                                 end
              | SOME (L.Num n) => A.Num n
              | SOME (L.Bool b) => A.Bool b
              | SOME (L.Id s) => A.Id s
              | SOME t => expected "bool, num or id" t
              | _ => err "unexpected end of input, expected bool, num or id")

       and exprs () : A.t =
           let
              (* check if token is in FIRST(expr) *)
              fun FIRSTexpr (L.Id _) = true
                | FIRSTexpr (L.Num _) = true
                | FIRSTexpr (L.Bool _) = true
                | FIRSTexpr L.If = true
                | FIRSTexpr L.Fn = true
                | FIRSTexpr L.Let = true
                | FIRSTexpr L.LParen = true
                | FIRSTexpr _ = false

              val ast1 = expr ()
           in
              if has () andalso FIRSTexpr (peek ())
                 then A.App (ast1, expr ())
              else ast1
           end
    in
       exprs ()
    end

end
