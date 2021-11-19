module Main where

import Syntax

main :: IO ()
main = do
    putStrLn $ "eval zero:   " ++ show (eval TmZero)
    putStrLn $ "eval true:   " ++ show (eval TmTrue)
    putStrLn $ "eval false:  " ++ show (eval TmFalse)
    putStrLn $ "eval succ n: " ++ show (eval TmZero)
    putStrLn $ "eval succ t: " ++ show (eval TmTrue)
    putStrLn $ "eval iszero: " ++ show (eval (TmIsZero TmTrue))
