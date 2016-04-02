# rogue-lang
Compiler of my programming language: Rogue

## Implementation details:
+ Implementation language: [Haskell](https://www.haskell.org/)
+ Target platform: [LLVM](http://llvm.org/)
+ Language specification: [Spec.pdf](https://github.com/ChShersh/rogue-lang/blob/master/spec/Spec.pdf)

## How to build and run
Use [stack](http://docs.haskellstack.org/en/stable/README/) build tool in this directory:

```
stack build
stack exec roguec
```

Produces LLVM-generated code written in test/testProgram.rg

### Supported features:
> Not yet implemented

### WIP:

#### Building
+ tests
+ more friendly run (at least with path to file)
+ [travis](https://travis-ci.org/) building
+ generate executables (or run result via lli)

#### Features
+ errors handling
  - monadic parser and lexer support for errors reporting
  - typechecker
  - semantic rules
+ global variables
+ power operator
+ make `scanf` to return value, not write in variable (stdlib)
+ type inference
+ lazy logical operations
+ immutable variables support
+ syntax sugar
+ HOF
+ custom start function

#### Known bugs:
+ not allowed function call assignment to variables
+ make `printf` and `scanf` work for booleans


### Compiler package structure
> TODO: describe sources