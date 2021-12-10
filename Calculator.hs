-- / 
-- Author : Yasser Bennani 
-- CW 2 Part 2 - Exam 
-- /

module Calculator where

import ParsingExt

-- Data type representing the calculator and its state
newtype Calculator = C (String, String) deriving Show

-- Evaluate calculator (c: calculation; m: memory)
evalCalc :: Calculator -> Calculator
evalCalc (C (c, m)) = (C (eval c, m))

-- Clear the last entry (CE)
clearEntry :: Calculator -> Calculator
clearEntry (C(r, m)) | r == "" = (C(r, m))
                     | isError r == True = (C("", m))   
                     | otherwise = (C(init r, m))

-- Initialize the calculator
initialize :: Calculator
initialize = (C ("0", "0"))

-- Clears the calculator (C)
reset :: Calculator -> Calculator
reset (C (c, m)) = (C ("", m))

-- Show the previous answer stored in memory 
showMem :: Calculator -> String                  
showMem (C (_, m))  | m == "0" = ""
                    | otherwise = m

-- Show the current answer
showCurrAns :: Calculator -> String
showCurrAns (C (c, _)) | c == "0" = ""
                       | otherwise = c

-- Change the sign of the current calculation
changeSign :: Calculator -> Calculator
changeSign (C (c, m)) = (C (show ((-1) * (read $ eval c) :: Double) , m))

-- Add result to memory (M+)
addMem :: Calculator -> Calculator
addMem (C (c, m)) | isError c == True = (C (c, m))
                  | otherwise = (C (c , eval (m ++ "+" ++ c )))

-- Subtract from value in memory (M-)
subMem :: Calculator -> Calculator
subMem (C (c, m)) | isError c == True = (C (c, m))
                  | otherwise = (C (c , eval (m ++ "-" ++ c )))

-- Store result to memory (MS)
storeMem :: Calculator -> Calculator
storeMem (C (c, m)) | c == "" = (C (c, c))
                    | isError c == True || isError (eval c) == True = (C (c, m))
                    | otherwise = (C (c, eval c))

-- Clear the last entry stored in memory (MC)
clearMem :: Calculator -> Calculator
clearMem (C (c, _)) = (C (c, "0"))

-- Recall result from memory (MR)
recallMem :: Calculator -> Calculator
recallMem (C (c, m)) = (C (c++m, m))

-- Add new input to the main calculator display
concatInput :: Calculator -> String -> Calculator
concatInput (C(r, m)) inp | r == "0" || isError r == True = (C(inp, m))
                          | otherwise = (C((r ++ inp), m))

-- e.g.
-- x = showCurrAns $ evalCalc (C("2/8*5","5"))
