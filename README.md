# MaML (isn't quite yet) An ML (compiler)

This is the front-end for an ML compiler, written in Standard ML. It consists of a lexer, a parser and a type checker (with Hindley-Milner style type inference). It's currently capable of parsing and type checking programs that are recognizably ML, e.g.

```
datatype 'a tree = Leaf of 'a
                 | Branch of 'a tree * 'a tree
val reflect =
 fn t =>
    case t of
        Leaf x => t
      | Branch (t1, t2) => Branch (reflect t2, reflect t1)
```

## syntax

Concrete syntax is mostly a subset of Standard ML, but diverges in a few to cut corners: no top-level function declarations, just value bindings to fns, value constructors must be capitalized (if it's good enough for Haskell...).

## building and testing

Requires SML/NJ and [QCheck/SML](http://contrapunctus.net/league/haques/qcheck/qcheck.html) for tests. Install QCheck by following the instructions there (likely need to edit ~/.smlnj-pathconfig) and then:

`make -f Makefile_parser test`

Would like to get it building under MLton, probably works but need to test and write build scripts (.mlb files).

## todo

### lexer

- Reset column numbers on new lines
- Read & lex files, as well as strings
- Comments

### parser

- type annotations on any expression
- Infix of 'a * string * 'a t * 'a t
                ^^^^^^ Infix should take a string for the op, not a binop
- return value of type `(SyntaxError, AST) either`
- open question: is it possible to remove Type.Paren ?

### type checker

- let expressions
- return value of type `(TypeError, AST) either`
- annotations
- patterns
