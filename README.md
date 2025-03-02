## Assignment 2

Familiarizing with Haskell language using 'cabal' package builder manager

Follwoing features have been added to FORTH interpreter

1.  The code has been changed in `Main.hs` so that, if the stack is not empty at the end of execution,  a message gets printed on the screen stating that stack is not empty and the stack content.
2. Following functionalities have been added to `Eval.hs` and corresponding unit tests have been writen under `EvalSpec.hs`
  * `EMIT`: takes a number from the stack and prints the character with the corresponding ASCI code
  * `CR`: prints a new line (for nice formating)
  * `STR`: converts the argument into a string (needs to work for all types)
  * `CONCAT2` and `CONCAT3` concatenates 2 or 3 strings from the stack (errors if arguments not strings)

3. Fuctional tests have been written in the test folder and the expected output has been written in .out file. 

## How to run the code
To build the project, use the following command:
```
cabal build
``` 
After building, you can run the unit tests with:
```
runhaskell EvalSpec.hs
```
Finally, to execute individual test cases, use the following command:
```
path\FORTH.exe tests/t1.4TH 
```

## Situations Encountered.

1. GHC Version Compatibility & Cross-Platform Build Issues: 
Initially, when I tried building the given project on macOS, I couldn't find a compatible GHC version. Therefore, I had to switch to a different operating system that the project supports in order to proceed.
2. Lazy Evaluation Complexity: 
Grasping Haskell's lazy evaluation model was challenging, especially since precise control over execution order and stack operations is essential in a Forth interpreter.
