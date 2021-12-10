-- / 
-- Author : Yasser Bennani 
-- CW 2 Part 2 - Exam 
-- /

module ParsingExt (module Parsing, evaluate, eval, isError) where
-- This module extends the Parsing module by Graham Hutton (modified)
-- and adds support for fractional numbers as well as evaluation 
-- expression based on grammar rules described below
import Parsing
import Text.Read (readMaybe)
import Data.List (isInfixOf)

-- Add support for parsing fractional numbers
dbl :: Parser Double
dbl = do number <- some digit
         char '.'
         fractional <- some digit
         return (read $ number ++ '.' : fractional)

double :: Parser Double
double = dbl
         <|> do char '-'
                d <- dbl
                return (-d)
         <|> integer

-- implementing addition
-- expr = expr + term | expr - term | term
expr :: Parser Double
expr = do t <- term
          symbol "+"
          e <- expr
          return (t + e)
       <|> do t <- term
              symbol "-"
              e <- expr
              return (t - e)
        <|> term

-- implementing multiplication
-- term = term * factor | term / factor | factor
term :: Parser Double
term = do f <- factor
          symbol "*"
          t <- term
          return (f * t)
       <|> do f <- factor
              symbol "/"
              t <- term
              return (f / t)
       <|> factor

-- implemeting support for parentheses
-- factor = (expr) | double
factor :: Parser Double
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> double

-- evaluate parsed expression using the parse
-- function from the imported Parsing library
eval :: String -> String
eval exp = case parse expr exp of
             [(a,[])]    -> show a
             [(_, i)]    -> incompleteErr ++ show i
             []          -> invalidErr


evaluate :: String -> Maybe Double
evaluate input = readMaybe $ eval input

incompleteErr, invalidErr :: String
incompleteErr = "Incomplete expression: "
invalidErr = "Invalid expression..."

isError :: String -> Bool
isError inp = isInfixOf incompleteErr inp || inp == invalidErr 