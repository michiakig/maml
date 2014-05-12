datatype 'a tree = Leaf of 'a
                 | Branch of 'a tree * 'a tree
val reflect =
 fn t =>
    case t of
        Leaf x => t
      | Branch (t1, t2) => Branch (reflect t2, reflect t1)
