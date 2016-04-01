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

### TODO:
+ Compile arbitrary file
+ tests