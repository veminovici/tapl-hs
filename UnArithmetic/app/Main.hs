module Main where

import Syntax

main :: IO ()
main = do
    let isNumerical = show . isNumericalVal 
    let isValue = show . isVal 
    let evaluate1 = show . eval1
    let evaluate = show . eval

    putStrLn $ "isnum zero:   " ++ isNumerical (TmZero "")
    putStrLn $ "isnum true:   " ++ isNumerical (TmTrue "")
    putStrLn $ "isnum false:  " ++ isNumerical (TmFalse "")
    putStrLn $ "isnum succ n: " ++ isNumerical (TmZero "")
    putStrLn $ "isnum succ t: " ++ isNumerical (TmTrue "")
    putStrLn $ "isnum iszero: " ++ isNumerical (TmIsZero "" $ TmTrue "")

    putStrLn $ "isval zero:   " ++ isValue (TmZero "")
    putStrLn $ "isval true:   " ++ isValue (TmTrue "")
    putStrLn $ "isval false:  " ++ isValue (TmFalse "")
    putStrLn $ "isval succ n: " ++ isValue (TmZero "")
    putStrLn $ "isval succ t: " ++ isValue (TmTrue "")
    putStrLn $ "isval iszero: " ++ isValue (TmIsZero "" $ TmTrue "")

    putStrLn $ "eval1 zero:   " ++ evaluate1 (TmZero "")
    putStrLn $ "eval1 true:   " ++ evaluate1 (TmTrue "")
    putStrLn $ "eval1 false:  " ++ evaluate1 (TmFalse "")
    putStrLn $ "eval1 succ n: " ++ evaluate1 (TmZero "")
    putStrLn $ "eval1 succ t: " ++ evaluate1 (TmTrue "")
    putStrLn $ "eval1 iszero: " ++ evaluate1 (TmIsZero "" $ TmTrue "")

    putStrLn $ "eval zero:   " ++ evaluate (TmZero "")
    putStrLn $ "eval true:   " ++ evaluate (TmTrue "")
    putStrLn $ "eval false:  " ++ evaluate (TmFalse "")
    putStrLn $ "eval succ n: " ++ evaluate (TmZero "")
    putStrLn $ "eval succ t: " ++ evaluate (TmTrue "")
    putStrLn $ "eval iszero: " ++ evaluate (TmIsZero "" $ TmTrue "")
