k-infix
=======

Run `raco docs k-infix` for in-depth documentation and customization.

```racket
(require k-infix)
($ 1 * 2 + 3 - 4)
```

## What about the other infix libraries? ##
There is [infix](https://pkgd.racket-lang.org/pkgn/package/heresy), but this works on strings and so is unhygienic. We would like to be able to work on the AST-level.
There is also [infix-syntax](https://pkgd.racket-lang.org/pkgn/package/infix-syntax) but it's horribly complicated and terribly documented. I've tried changing that but it becomes too complicated for me.

This package is a very simple extended shunting-yard algorithm that handles n-ary functions, binary, and unary operators, and arbitrary expressions.
