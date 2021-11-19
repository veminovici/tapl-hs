module Main where

import Text.Parsec(ParseError)
import Parser
import Syntax

pevaluate :: String -> (String, Either ParseError Term)
pevaluate s = (s, peval s)

showRes :: (String, Either ParseError Term) -> String
showRes (s, e) = s ++ " ===> " ++ show e

prnterm :: String -> String
prnterm = showRes . pevaluate

main :: IO ()
main = do
    putStrLn $ prnterm "if (if zero? 0 then true else false) then true else false"
    putStrLn $ prnterm "if false then 0 else succ (pred (succ 0))"
    putStrLn $ prnterm "if true then true else (if false then false else false)"
    putStrLn $ prnterm "if (zero? (if (zero? 0) then 0 else (succ 0))) then succ 0 else succ (succ 0)"
