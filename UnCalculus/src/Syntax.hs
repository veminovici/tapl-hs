module Syntax where

import Context

type CtxLength = Int

data Term
    = TmValue DBIndex CtxLength
    | TmAbs Hint Term
    | TmApp Term Term
    deriving Show

showTerm :: Context -> Term -> String
showTerm ctx (TmValue ndx _) = 
    getVarName ndx ctx

showTerm ctx (TmAbs hint t) = 
    let (n', ctx') = freshVarName hint ctx 
    in "(lambda " ++ n' ++ ". " ++ showTerm ctx' t ++ ")"

showTerm ctx (TmApp t1 t2) = 
    "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
