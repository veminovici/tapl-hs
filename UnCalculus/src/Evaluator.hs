module Evaluator where

import Control.Monad
import Context
import Syntax

shiftTerm :: DBIndex -> Term -> Term
shiftTerm d = walk 0
    where walk c (TmValue ndx len)
            | ndx > c = TmValue (ndx + d) (len + d)
            | otherwise  = TmValue ndx (len + d)
          walk c (TmAbs hint t) = TmAbs hint (walk (c + 1) t)
          walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

subTerm :: DBIndex -> Term -> Term -> Term
subTerm i s = walk 0
    where  walk c (TmValue ndx len)
            | ndx == i + c = s
            | otherwise = TmValue ndx len
           walk c (TmAbs hint t) = TmAbs hint (walk (c + 1) t)
           walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

subTop :: Term -> Term -> Term
subTop s t = shiftTerm (-1) (subTerm 0 (shiftTerm 1 s) t)

isValue :: Term -> Bool
isValue (TmValue _ _) = True
isValue _             = False

eval1 :: Term -> Maybe Term
eval1 (TmApp (TmAbs _ t12) v2)
    | isValue v2 = return $ subTop v2 t12
eval1 (TmApp t1 t2)
    | isValue t1 = liftM2 TmApp (return t1) (eval1 t2)
    | otherwise = liftM2 TmApp (eval1 t1) (return t2)
eval1 _ = Nothing

eval :: Term -> Term
eval t =
    maybe t eval (eval1 t)
