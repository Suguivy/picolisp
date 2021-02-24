# picolisp

A very simple lisp to learn about interpreters, Lisp and its possibilities.

The BNF grammar will be:

```
SExpr = AtomicSym | '(' SExpr '.' SExpr ')' | List
List  = '(' SExpr* ')'
AtomicSym = Letter AtomPart
Letter = 'a' | 'b' | ... | 'z'
Number = Digit*
Digit = '0' | '1' | ... | '9'
```
...

