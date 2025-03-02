-- HSpec tests for Val.hs
-- Execute: runhaskell EvalSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    context "*" $ do
        it "multiplies integers" $ do
            eval "*" [Integer 2, Integer 3] `shouldBe` [Integer 6]
        
        it "multiplies floats" $ do
            eval "*" [Integer 2, Real 3.0] `shouldBe` [Real 6.0]
            eval "*" [Real 3.0, Integer 3] `shouldBe` [Real 9.0]
            eval "*" [Real 4.0, Real 3.0] `shouldBe` [Real 12.0]

        it "errors on too few arguments" $ do   
            evaluate (eval "*" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "*" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

        -- this does not work, seems to be a HSpec bug
        -- it "errors on non-numeric inputs" $ do
        --    evaluate(eval "*" [Real 3.0, Id "x"]) `shouldThrow` anyException

    context "DUP" $ do
        it "duplicates values" $ do
            eval "DUP" [Integer 2] `shouldBe` [Integer 2, Integer 2]
            eval "DUP" [Real 2.2] `shouldBe` [Real 2.2, Real 2.2]
            eval "DUP" [Id "x"] `shouldBe` [Id "x", Id "x"]

        it "errors on empty stack" $ do
            evaluate (eval "DUP" []) `shouldThrow` errorCall "Stack underflow"   


    -- Addition Tests
    context "+" $ do
        it "adds two integers" $ do
            eval "+" [Integer 2, Integer 3] `shouldBe` [Integer 5]

        it "adds a float and an integer" $ do
            eval "+" [Real 2.5, Integer 3] `shouldBe` [Real 5.5]
            eval "+" [Integer 3, Real 2.5] `shouldBe` [Real 5.5]

        it "adds two floats" $ do
            eval "+" [Real 1.5, Real 2.5] `shouldBe` [Real 4.0]

        it "errors on too few arguments" $ do
            evaluate (eval "+" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "+" [Integer 3]) `shouldThrow` errorCall "Stack underflow"

   
    -- Subtraction Tests
    context "-" $ do
        it "subtracts two integers" $ do
            eval "-" [Integer 2, Integer 5] `shouldBe` [Integer 3]

        it "subtracts a float from an integer" $ do
            eval "-" [Real 2.5, Integer 5] `shouldBe` [Real 2.5]
            eval "-" [Integer 5, Real 2.5] `shouldBe` [Real (-2.5)]

        it "subtracts two floats" $ do
            eval "-" [Real 5.5, Real 3.0] `shouldBe` [Real (-2.5)]

        it "errors on too few arguments" $ do
            evaluate (eval "-" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "-" [Integer 3]) `shouldThrow` errorCall "Stack underflow"

    
    -- Division Tests
    context "/" $ do
        it "performs integer division" $ do
            eval "/" [Integer 2, Integer 10] `shouldBe` [Integer 5]
            eval "/" [Integer 3, Integer 9] `shouldBe` [Integer 3]

        it "performs float division" $ do
            eval "/" [Real 2.0, Integer 10] `shouldBe` [Real 5.0]
            eval "/" [Integer 5, Real 10.0] `shouldBe` [Real 2.0]
            eval "/" [ Real 3.0, Real 7.5] `shouldBe` [Real 2.5]

        it "errors on division by zero" $ do
            evaluate (eval "/" [Integer 5, Integer 0]) `shouldThrow` errorCall "Division by zero"
            evaluate (eval "/" [Real 0.0, Real 5.0]) `shouldThrow` errorCall "Division by zero"

        it "errors on too few arguments" $ do
            evaluate (eval "/" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "/" [Integer 3]) `shouldThrow` errorCall "Stack underflow"

    
    -- Power Tests
    context "^" $ do
        it "computes integer power" $ do
            eval "^" [Integer 2, Integer 3] `shouldBe` [Integer 9]
            eval "^" [Integer 3, Integer 2] `shouldBe` [Integer 8]

        it "computes float power" $ do
            eval "^" [Real 2.0, Integer 3] `shouldBe` [Real 9.0]
            eval "^" [Integer 3, Real 2.0] `shouldBe` [Real 8.0]
            eval "^" [Real 2.0, Real 2.5] `shouldBe` [Real 6.25]

        it "errors on negative exponent for integers" $ do
            evaluate (eval "^" [Integer (-2), Integer 4]) `shouldThrow` errorCall "Negative exponents are not supported for integers"

        it "errors on too few arguments" $ do
            evaluate (eval "^" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "^" [Integer 3]) `shouldThrow` errorCall "Stack underflow"


 
    -- EMIT Tests
    context "EMIT" $ do
        it "converts an integer to an ASCII character" $ do
            evalOut "EMIT" ([Integer 65], "") `shouldBe` ([], "A")
            evalOut "EMIT" ([Integer 97], "") `shouldBe` ([], "a")

        it "throws an error on invalid ASCII codes" $ do
            evaluate (evalOut "EMIT" ([Integer (-1)], "")) `shouldThrow` errorCall "Invalid ASCII code"
            evaluate (evalOut "EMIT" ([Integer 300], "")) `shouldThrow` errorCall "Invalid ASCII code"

        it "throws an error when stack is empty" $ do
            evaluate (evalOut "EMIT" ([], "")) `shouldThrow` errorCall "Stack underflow"


    -- CR Tests
    context "CR" $ do
        it "adds a newline character" $ do
            evalOut "CR" ([], "Hello") `shouldBe` ([], "Hello\n")


    -- STR Tests
    context "STR" $ do
        it "converts an integer to a string" $ do
            evalOut "STR" ([Integer 123], "") `shouldBe` ([Id "123"], "")

        it "converts a real number to a string" $ do
            evalOut "STR" ([Real 45.67], "") `shouldBe` ([Id "45.67"], "")

        it "converts an identifier string to itself" $ do
            evalOut "STR" ([Id "hello"], "") `shouldBe` ([Id "hello"], "")

        it "throws an error when stack is empty" $ do
            evaluate (evalOut "STR" ([], "")) `shouldThrow` errorCall "Stack underflow"


    -- CONCAT2 Tests
    context "CONCAT2" $ do
        it "concatenates two strings" $ do
            evalOut "CONCAT2" ([Id "world", Id "hello"], "") `shouldBe` ([Id "helloworld"], "")

        it "throws an error if arguments are not strings" $ do
            evaluate (evalOut "CONCAT2" ([Integer 5, Id "hello"], "")) `shouldThrow` errorCall "Stack underflow or non-string argument"

        it "throws an error when stack has too few arguments" $ do
            evaluate (evalOut "CONCAT2" ([Id "onlyone"], "")) `shouldThrow` errorCall "Stack underflow or non-string argument"


    -- CONCAT3 Tests
    context "CONCAT3" $ do
        it "concatenates three strings" $ do
            evalOut "CONCAT3" ([Id "!", Id "world", Id "hello"], "") `shouldBe` ([Id "helloworld!"], "")

        it "throws an error if arguments are not strings" $ do
            evaluate (evalOut "CONCAT3" ([Id "!", Integer 5, Id "hello"], "")) `shouldThrow` errorCall "Stack underflow or non-string argument"

        it "throws an error when stack has too few arguments" $ do
            evaluate (evalOut "CONCAT3" ([Id "onlyone", Id "two"], "")) `shouldThrow` errorCall "Stack underflow or non-string argument"


  describe "evalOut" $ do
      context "." $ do
        it "prints top of stack" $ do
            evalOut "." ([Id "x"], "") `shouldBe` ([],"x")
            evalOut "." ([Integer 2], "") `shouldBe` ([], "2")
            evalOut "." ([Real 2.2], "") `shouldBe` ([], "2.2")

        it "errors on empty stack" $ do
            evaluate(evalOut "." ([], "")) `shouldThrow` errorCall "Stack underflow"

      it "eval pass-through" $ do
         evalOut "*" ([Real 2.0, Integer 2], "blah") `shouldBe` ([Real 4.0], "blah") 