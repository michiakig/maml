structure Syntax =
struct

structure Concrete = struct
   (* not really "concrete" syntax, but is easier to write *)
   datatype t = Num of int
              | Bool of bool
              | Succ of t
              | Pred of t
              | IsZero of t
              | If of t * t * t
              | App of t * t
              | Fun of string * t
              | Id of string
end

structure Abstract = struct

   structure C = Concrete

   datatype t = Num of int * int
              | Bool of int * bool
              | Succ of int * t
              | Pred of int * t
              | IsZero of int * t
              | If of int * t * t * t
              | App of int * t * t
              (* functions need two ids, one for the bound var,
               * one for the fun expr itself *)
              | Fun of int * int * string * t
              | Id of int * string

   (* given an ast node and an id, find the node in this tree with that id *)
   fun findByid (n as Num (id, _), id') = if id = id' then SOME n else NONE
     | findByid (n as Bool (id, _), id') = if id = id' then SOME n else NONE
     | findByid (n as Succ (id, _), id') = if id = id' then SOME n else NONE
     | findByid (n as Pred (id, _), id') = if id = id' then SOME n else NONE
     | findByid (n as IsZero (id, _), id') = if id = id' then SOME n else NONE
     | findByid (n as If (id, e1, e2, e3), id') =
       if id = id'
       then SOME n
       else (case findByid (e1, id') of
                 SOME n => SOME n
               | NONE => case findByid (e2, id') of
                             SOME n => SOME n
                           | NONE => case findByid (e3, id') of
                                         SOME n => SOME n
                                       | NONE => NONE)
     | findByid (n as App (id, e1, e2), id') =
       if id = id'
       then SOME n
       else (case findByid (e1, id') of
                 SOME n => SOME n
               | NONE => case findByid (e2, id') of
                             SOME n => SOME n
                           | NONE => NONE)
     | findByid (n as Fun (b, f, v, e), id') =
       if id' = b
       then SOME (Id (b, v))
       else if id' = f
       then SOME n
       else (case findByid (e, id') of
                 SOME n => SOME n
               | NONE => NONE)
     | findByid (n as Id (id, _), id') = if id = id' then SOME n else NONE

   (* get this ast node's id *)
   fun getId (Num (id, _))       = id
     | getId (Bool (id, _))      = id
     | getId (Succ (id, _))      = id
     | getId (Pred (id, _))      = id
     | getId (IsZero (id, _))    = id
     | getId (If (id, _, _, _))  = id
     | getId (App (id, _, _))    = id
     | getId (Fun (_, id, _, _)) = id
     | getId (Id (id, _))        = id

   fun show (Bool (id, b)) = "Bool " ^ Int.toString id ^ "," ^ Bool.toString b
     | show (Num (id, n)) = "Num " ^ Int.toString id ^ "," ^ Int.toString n
     | show (Succ (id, e)) = "Succ (" ^ Int.toString id ^ "," ^ show e ^ ")"
     | show (Pred (id, e)) = "Pred (" ^ Int.toString id ^ "," ^ show e ^ ")"
     | show (IsZero (id, e)) = "IsZero (" ^ Int.toString id ^ "," ^ show e ^ ")"
     | show (If (id, e1, e2, e3)) =
       "If (" ^ Int.toString id ^ ","
       ^ show e1 ^ "," ^ show e2 ^ "," ^ show e3 ^ ")"
     | show (App (id, e1, e2)) = "App (" ^ Int.toString id ^ "," ^ show e1 ^ ","
                                                   ^ show e2 ^ ")"
     | show (Fun (id1, id2, x, e)) =
       "Fun (" ^ Int.toString id1 ^ "," ^ Int.toString id2 ^
       "," ^ x ^ "," ^ show e ^ ")"
     | show (Id (id, x)) = "Id (" ^ Int.toString id ^ "," ^ x ^ ")"

   local
      val id = ref 0
   in
      fun newId () = !id before id := !id + 1
      fun reset () = id := 0
   end

   (* keep track of lexically bound variables (in the obj lang)
    * assign Id nodes referring to the same bound var the same ast id *)
   structure Env = BinaryMapFn(struct
      type ord_key = string
      val compare = String.compare
   end)

   exception FreeVariable

   fun makeAst (e : C.t) : t =
       let
          fun makeAst' (env, e) =
              case e of
                  (C.Num n) => Num (newId (), n)
                | (C.Bool b) => Bool (newId (), b)
                | (C.Succ e1) => Succ (newId (), makeAst' (env, e1))
                | (C.Pred e1) => Pred (newId (), makeAst' (env, e1))
                | (C.IsZero e1) => IsZero (newId (), makeAst' (env, e1))
                | (C.If (e1, e2, e3)) => If (newId (), makeAst' (env, e1), makeAst' (env, e2), makeAst' (env, e3))
                | (C.App (e1, e2)) => App (newId (), makeAst' (env, e1), makeAst' (env, e2))
                | (C.Fun (x, e1)) =>
                  let
                     (* id for bound var *)
                     val id = newId ()
                     (* recur on body expr, extending the env *)
                     val e1' = makeAst' (Env.insert (env, x, id), e1)
                  in
                     (* assign fun expr fresh id, plus id for bound var *)
                     Fun (id, newId (), x, e1')
                  end
                | (C.Id x) =>
                  (* look up the bound var in the env,
                   * use id generated by enclosing lexical fun expr *)
                  case Env.find (env, x) of
                      SOME id => Id (id, x)
                    | NONE => raise FreeVariable
       in
          makeAst' (Env.empty, e)
       end

end

end
