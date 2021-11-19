module Parser(parseTerm) where

import Syntax
import Text.Parsec
import Text.Parsec.String


parseTrue :: Parser Term
parseTrue = string "true" >> return TmTrue

parseFalse :: Parser Term
parseFalse = string "false" >> return TmFalse

parseZero :: Parser Term
parseZero = string "0" >> return TmZero

parseIsZero :: Parser Term
parseIsZero = do 
    string "zero?"
    space
    TmIsZero <$> parseTerm

parseIf :: Parser Term
parseIf = do 
    string "if"
    space
    predicate <- parseTerm
    space
    string "then"
    space
    consequent <- parseTerm
    space
    string "else"
    space
    TmIf predicate consequent <$> parseTerm

parseSucc :: Parser Term
parseSucc = do 
    string "succ"
    space
    spaces
    TmSucc <$> parseTerm

parsePred :: Parser Term
parsePred = do 
    string "pred"
    space
    spaces
    TmPred <$> parseTerm

parseTerm :: Parser Term
parseTerm =
  parseTrue   <|>
  parseFalse  <|>
  parseIf     <|>
  parseZero   <|>
  parseSucc   <|>
  parsePred   <|>
  parseIsZero <|>
  between (string "(") (string ")") parseTerm
