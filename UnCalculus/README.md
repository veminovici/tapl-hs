# Untyped Lambda Calculus

### Resources
[Types and Programming Languages]((http://www.cis.upenn.edu/~bcpierce/tapl/)) **Chapters 6 & 7**

### Syntax

```haskell
data Term
    = TmValue DBIndex CtxLength
    | TmAbs Hint Term
    | TmApp Term Term
```

### Context

```haskell
-- | de Bruijn index
type DBIndex = Int
-- | Hint for the name of the variable
type Hint = String
-- | Bund variable names
type Context = [String]
```
