# MaML (isn't quite yet) An ML (compiler)

This is a work-in-progress ML compiler, written in Standard ML. It consists of most of a front-end: a lexer, a parser, a type checker (along with Hindley-Milner type inference), and a naive pattern compiler. It's capable of parsing and type checking programs that are recognizably ML, e.g.

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

The parser accepts a concrete syntax that is mostly a subset of Standard ML, but diverges in a few places to cut corners: no top-level function declarations, just value bindings, constructors must be capitalized (if it's good enough for Haskell...), only pattern matching `case` expressions, no patterns in functions or lambdas, no syntactic sugar for lists, ... The type checker doesn't require `rec` after recursive value bindings (see `reflect` above)

## building and testing

Requires [SML/NJ](http://smlnj.org/) and [QCheck/SML](http://contrapunctus.net/league/haques/qcheck/qcheck.html). To install QCheck, clone [this repo](https://github.com/league/qcheck) and add `QCHECK /path/to/qcheck/clone` to your `~/.smlnj-pathconfig`.

Use `make test` to compile and run the tests; see the Makefile for more targets.

It should build under MLton and other Standard ML compilers, as it doesn't use any language extensions, but this still needs to be tested and MLton `.mlb` files need to be written to make this easier.

## todos

### lexer

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

### desugaring

- integrate the [current pattern match compiler](https://github.com/spacemanaki/maml/blob/master/src/desugar.sml) (from the [The Implementation of Functional Programming Languages](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/) with the rest of the code

## contributing

Feel free to fork and work on any of the above, however get in touch beforehand as I have WIP branches for several bugs and features.

## disambiguation

Not to be confused with [MAML](http://en.wikipedia.org/wiki/Microsoft_Assistance_Markup_Language).
