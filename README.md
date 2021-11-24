# Simplee::Principia::Tapl

## Resources
[Types and Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/)

## Untyped Arithmetic
This is an impementation of an untyped arithmetic language, based on the chapters 3 and 4 of the book.

#### Build the [Project](https://github.com/veminovici/tapl-hs/tree/master/UnArithmetic)
```bash
cd UnArithmetic
stack build
stack run
```

#### Example
```haskell
import Parser
import Syntax

peval "if (if zero? 0 then true else false) then true else false"
```

## Untyped Lambda Calculus

#### Build the [Project](https://github.com/veminovici/tapl-hs/tree/master/UnCalculus)
```bash
cd UnCalculus
stack build
stack run
```
