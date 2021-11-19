module Syntax (
    eval,
    eval1,
    Term(..) ) where

import Control.Monad

data Term
    = TmFalse               -- | false value
    | TmTrue                -- | true value
    | TmIf Term Term Term   -- | if t1 then t2 else t3
    | TmZero                -- | zero value
    | TmSucc Term           -- | successor
    | TmPred Term           -- | predecesor
    | TmIsZero Term         -- | iszero t
    deriving Show

-- | Returns True if the term is a numerical value.
isNumericalVal :: Term -> Bool
isNumericalVal TmZero = True
isNumericalVal (TmSucc t) = isNumericalVal t
isNumericalVal _ = False

-- | Returns True if the term is a value.
isVal :: Term -> Bool
isVal TmFalse = True
isVal TmTrue = True
isVal t | isNumericalVal t = True
isVal _ = False

-- | One-step evaluation for a term
eval1 :: Term -> Maybe Term
eval1 TmTrue = Nothing
eval1 TmFalse = Nothing
eval1 (TmIf TmTrue  t1 _) = return t1
eval1 (TmIf TmFalse _ t2) = return t2
eval1 (TmIf t1 t2 t3) = liftM3 TmIf (eval1 t1) (return t2) (return t3)
eval1 TmZero = Nothing
eval1 (TmSucc t1) = fmap TmSucc (eval1 t1)
eval1 (TmPred TmZero) = Just TmZero
eval1 (TmPred (TmSucc t)) | isNumericalVal t = Just t
eval1 (TmPred t1) = fmap TmPred (eval1 t1)
eval1 (TmIsZero TmZero) = Just TmTrue
eval1 (TmIsZero (TmSucc t)) | isNumericalVal t = Just TmFalse
eval1 (TmIsZero t1) = fmap TmIsZero (eval1 t1)

-- | Full evaluation
eval :: Term -> Term
eval t =
    maybe t eval (eval1 t)

