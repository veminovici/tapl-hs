module Main where

import Text.Parsec
import Parser
import Syntax

peval :: String -> Either ParseError Term
peval s = eval <$> parse parseTerm "arith" s

prntEval :: String -> IO ()
prntEval s = putStrLn $ s ++ " => " ++ show (peval s)

main :: IO ()
main = do
    prntEval "if (if zero? 0 then true else false) then true else false"
    prntEval "if false then 0 else succ (pred (succ 0))"
    prntEval "if true then succ 0 else 1"
    prntEval "if true then true else (if false then false else false)"
    prntEval "if (zero? (if (zero? 0) then 0 else (succ 0))) then succ 0 else succ (succ 0)"
