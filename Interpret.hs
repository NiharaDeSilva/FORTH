module Interpret where
-- This file contains the FORTH interpreter

import Val
import Eval (evalOut)  -- Import evalOut
import Flow
import qualified Data.Map as Map

-- We use UserDict to store user-defined function definitions.
type UserDict = Map.Map String [String]

evalSequence :: UserDict -> [String] -> [Val] -> String -> ([Val], String)
evalSequence _ [] stack out = (stack, out)
evalSequence dict (w:ws) stack out =
    let (_, newStack, newOut) = evalF (dict, stack, out) (strToVal w)
    in evalSequence dict ws newStack newOut


-- evalF processes one token (already converted to a Val) and updates (dict, stack, out).
evalF :: (UserDict, [Val], String) -> Val -> (UserDict, [Val], String)
-- Ignore function-definition markers.
evalF (dict, stack, out) (Id ":") = (dict, stack, out)
evalF (dict, stack, out) (Id ";") = (dict, stack, out)
-- If the token is an identifier, check if it is a user-defined function.
evalF (dict, stack, out) (Id name)
    | Map.member name dict =
        let funcBody = Map.findWithDefault [] name dict
            (newStack, newOut) = evalSequence dict funcBody stack out
        in (dict, newStack, newOut)
    | otherwise = evalOutWithDict dict name (stack, out)
-- For non-identifiers (numbers, etc.), simply push them onto the stack.
evalF (dict, stack, out) val = (dict, val : stack, out)

-- For built-in operators, delegate to evalOut.
evalOutWithDict :: UserDict -> String -> ([Val], String) -> (UserDict, [Val], String)
evalOutWithDict dict op (stack, out) =
    let (newStack, newOut) = evalOut op (stack, out)
    in (dict, newStack, newOut)

-- The main interpret function: note that we work with tokens as Strings.
interpret :: String -> ([Val], String)
interpret text =
    let tokens = words text  -- Do not convert here—keep them as raw strings
        (_, finalStack, finalOutput) = interpretWords tokens Map.empty ([], "")
    in (finalStack, finalOutput)

-- interpretWords processes a list of tokens, handling function definitions.
interpretWords :: [String] -> UserDict -> ([Val], String) -> (UserDict, [Val], String)
interpretWords [] dict (stack, out) = (dict, stack, out)
-- When ":" is encountered, store the function definition in the dictionary.
interpretWords (":":name:body) dict (stack, out) =
    let functionBody = takeWhile (/= ";") body      -- Extract the function body tokens
        newDict = Map.insert name functionBody dict  -- Store under the function name
        remainingWords = drop (length functionBody + 1) body  -- Skip past the definition and ";"
    in interpretWords remainingWords newDict (stack, out)
-- Otherwise, process the next token.
interpretWords (word:ws) dict (stack, out) =
    let tokenVal = strToVal word  -- Convert the token to a Val (numbers become Integer/Real, etc.)
        (newDict, newStack, newOut) = evalF (dict, stack, out) tokenVal
    in interpretWords ws newDict (newStack, newOut)











-- module Interpret where
-- -- this file contains the FORTH interpreter

-- import Val
-- import Eval
-- import Flow

-- -- inner function for foldl
-- -- Takes the current stack and an input and 
-- -- computes the next stack
-- evalF :: ([Val], String) -> Val -> ([Val], String)
-- evalF s (Id op) = evalOut op s
-- -- cannot run, put on the stack and preserve output
-- evalF (s, out) x = (x:s,out)

-- -- function to interpret a string into a stack and 
-- -- an output string
-- interpret :: String -> ([Val], String)
-- interpret text = text |>
--     words |> -- brake text into words
--     map strToVal |> -- strings to instructions
--     foldl evalF ([], "") -- perform evaluation
