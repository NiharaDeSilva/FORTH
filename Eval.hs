module Eval where
-- This file contains definitions for functions and operators

import Val
import Data.Char (chr)

-- main evaluation function for operators and 
-- built-in FORTH functions with no output
-- takes a string and a stack and returns the stack
-- resulting from evaluation of the function
eval :: String -> [Val] -> [Val]
-- Multiplication
-- if arguments are integers, keep result as integer
eval "*" (Integer x: Integer y:tl) = Integer (x*y) : tl
-- if any argument is float, make result a float
eval "*" (x:y:tl) = (Real $ toFloat x * toFloat y) : tl 
-- any remaining cases are stacks too short
eval "*" _ = error("Stack underflow")



-- Addition
-- if arguments are integers, keep result as integer
eval "+" (Integer x: Integer y:tl) = Integer (x+y) : tl
-- if any argument is float, make result a float
eval "+" (x:y:tl) = (Real $ toFloat x + toFloat y) : tl 
-- any remaining cases are stacks too short
eval "+" _ = error("Stack underflow")



-- Subtraction
-- if arguments are integers, keep result as integer
eval "-" (Integer x: Integer y:tl) = Integer (y-x) : tl
-- if any argument is float, make result a float
eval "-" (x:y:tl) = (Real $ toFloat y - toFloat x) : tl 
-- any remaining cases are stacks too short
eval "-" _ = error("Stack underflow")



-- Division
-- if arguments are integers, keep result as integer
eval "/" (Integer y: Integer x:tl)
    | x == 0 = error "Division by zero"
    | mod y x == 0 = Integer (y `div` x) : tl
    | otherwise = Real (fromIntegral y / fromIntegral x) : tl
eval "/" (y:x:tl)
    | toFloat x == 0 = error "Division by zero"
    | otherwise = Real (toFloat y / toFloat x) : tl
eval "/" _ = error("Stack underflow")



-- Power (^)
-- if arguments are integers, keep result as integer
eval "^" (Integer x: Integer y:tl) 
    | x < 0 = error "Negative exponents are not supported for integers"
    | otherwise = Integer (y ^ x) : tl
eval "^" (x:y:tl) = (Real $ toFloat y ** toFloat x) : tl 
eval "^" _ = error("Stack underflow")



-- Duplicate the element at the top of the stack
eval "DUP" (x:tl) = (x:x:tl)
eval "DUP" [] = error("Stack underflow")

-- this must be the last rule
-- it assumes that no match is made and preserves the string as argument
eval s l = Id s : l 


-- variant of eval with output
-- state is a stack and string pair
evalOut :: String -> ([Val], String) -> ([Val], String) 

-- EMIT: Converts an integer into an ASCII character
evalOut "EMIT" (Integer x:tl, out)
    | x < 0 || x > 255 = error "Invalid ASCII code"
    | otherwise = (tl, out ++ [chr x])
evalOut "EMIT" _ = error "Stack underflow"


-- CR: Prints a newline
evalOut "CR" (stack, out) = (stack, out ++ "\n")


-- STR: Converts any type into a string
evalOut "STR" (Integer x:tl, out) = (Id (show x) : tl, out)
evalOut "STR" (Real x:tl, out) = (Id (show x) : tl, out)
evalOut "STR" (Id s:tl, out) = (Id s : tl, out)
evalOut "STR" _ = error "Stack underflow"


-- CONCAT2: Concatenates two strings
evalOut "CONCAT2" (Id s1 : Id s2 : tl, out) = (Id (s2 ++ s1) : tl, out)
evalOut "CONCAT2" _ = error "Stack underflow or non-string argument"



-- CONCAT3: Concatenates three strings
evalOut "CONCAT3" (Id s1 : Id s2 : Id s3 : tl, out) = (Id (s3 ++ s2 ++ s1) : tl, out)
evalOut "CONCAT3" _ = error "Stack underflow or non-string argument"



-- print element at the top of the stack
evalOut "." (Id x:tl, out) = (tl, out ++ x)
evalOut "." (Integer i:tl, out) = (tl, out ++ (show i))
evalOut "." (Real x:tl, out) = (tl, out ++ (show x))
evalOut "." ([], _) = error "Stack underflow"



-- this has to be the last case
-- if no special case, ask eval to deal with it and propagate output
evalOut op (stack, out) = (eval op stack, out)