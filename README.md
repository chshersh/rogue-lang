# rogue-lang [![Build Status](https://travis-ci.org/ChShersh/rogue-lang.svg?branch=master)](https://travis-ci.org/ChShersh/rogue-lang)
Compiler of my programming language: Rogue

## Implementation details:
+ Implementation language: [Haskell](https://www.haskell.org/)
+ Target platform: [LLVM](http://llvm.org/)
+ Language specification: [Spec.pdf](https://github.com/ChShersh/rogue-lang/blob/master/spec/Spec.pdf)

## How to build and run
Use [stack](http://docs.haskellstack.org/en/stable/README/) build tool in this directory:

```
stack build
stack exec roguec -- --file <file name> [--verbose]
```

#### Features
+ [ ] errors handling
  - [x] monadic parser and lexer support for errors reporting
  - [ ] typechecker
  - [ ] semantic rules
+ [ ] global variables
+ [ ] power operator
+ [ ] make `scanf` return value, not write in variable (stdlib)
+ [ ] type inference
+ [ ] lazy logical operations
+ [ ] immutable variables (and passing to function by reference) support
+ [ ] HOF
+ [ ] pattern matching syntax sugar
+ [ ] default arguments
+ [ ] custom start function

#### Known bugs:
+ [ ] make `printf` and `scanf` work for booleans
+ [ ] not allowed linebreaks in expressions

### Compiler package structure
> TODO: describe sources
