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

Concrete syntax is mostly a subset of Standard ML, but diverges in a few places to cut corners: no top-level function declarations, just value bindings to fns, value constructors must be capitalized (if it's good enough for Haskell...).

## building and testing

Requires [SML/NJ](http://smlnj.org/) and [QCheck/SML](http://contrapunctus.net/league/haques/qcheck/qcheck.html); make sure to add `QCHECK /path/to/qcheck` to your `~/.smlnj-pathconfig`.

Run `make` for the default build; see the Makefile for more targets.

Would like to get it building under MLton, which probably works as I tend to carefully avoid ML language extensions, but still need to test and (crucially) write MLton build scripts (.mlb files).

## todos

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
