module Syntax
    ( Term(..)
    , eval1
    , eval
    ) where

import Control.Monad

data Term                   -- | Terminals
    = TmFalse               -- | FALSE value
    | TmTrue                -- | TRUE value
    | TmIf Term Term Term   -- | IF expression
    | TmZero                -- | ZERO value
    | TmSucc Term           -- | Successor
    | TmPred Term           -- | Predecessor
    | TmIsZero Term         -- | Zero test
    deriving (Show)

-- | Returns the number of constants in a given term
consts :: Term -> Int
consts TmFalse = 1
consts TmTrue = 1
consts (TmIf t1 t2 t3) = consts t1 + consts t2 + consts t3
consts TmZero = 1
consts (TmSucc t) = consts t
consts (TmPred t) = consts t
consts (TmIsZero t) = consts t

-- | Returns the size (total number of sub-terms) for a given term
size :: Term -> Int
size TmFalse = 1
size TmTrue = 1
size (TmIf t1 t2 t3) = 1 + size t1 + size t2 + size t3
size TmZero = 1
size (TmSucc t) = 1 + size t
size (TmPred t) = 1 + size t
size (TmIsZero t) =  1 + size t

-- | Returns the depth of a term.
depth :: Term -> Int
depth TmFalse = 1
depth TmTrue = 1
depth (TmIf t1 t2 t3) = let xs = map depth [t1, t2, t3] in maximum xs
depth TmZero = 1
depth (TmSucc t) = 1 + depth t
depth (TmPred t) = 1 + depth t
depth (TmIsZero t) = 1 + depth t

-- | Returns true if a given term is a numeric value
isNumericalValue :: Term -> Bool
isNumericalValue TmZero = True
isNumericalValue (TmSucc t) = isNumericalValue t
isNumericalValue _ = False

-- | Returns the new term based on the one-step evaludation of a given term.
-- If the given term cannot be evaluated (normal form), it returns Nothing.
eval1 :: Term -> Maybe Term
eval1 TmFalse = Nothing
eval1 TmTrue = Nothing
eval1 (TmIf TmTrue t1 _) = Just t1
eval1 (TmIf TmFalse _ t2) = Just t2
eval1 (TmIf t1 t2 t3) = liftM3 TmIf (eval1 t1) (return t2) (return t3)
eval1 TmZero = Nothing
eval1 (TmSucc t) = fmap TmSucc (eval1 t)
eval1 (TmPred TmZero) = Just TmZero
eval1 (TmPred (TmSucc t)) | isNumericalValue t = Just t
eval1 (TmPred t) = fmap TmPred (eval1 t)
eval1 (TmIsZero TmZero) = Just TmZero
eval1 (TmIsZero (TmSucc t)) | isNumericalValue t = Just t
eval1 (TmIsZero t) = fmap TmIsZero (eval1 t)

-- | Runs the multi-step evaluation for a given tem
eval :: Term -> Term
eval t = maybe t eval (eval1 t)
