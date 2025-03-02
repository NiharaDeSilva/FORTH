## Assignment 2

Familiarizing with Haskell language using 'cabal' package builder manager

Follwoing features have been added to FORTH interpreter

1.  The code has been changed in `Main.hs` so that, if the stack is not empty at the end of execution,  a message gets printed on the screen stating that stack is not empty and the stack content.
2. Following functionalities have been added to `Eval.hs` and unit tests have been writen under `EvalSpec.hs`
  * `EMIT`: takes a number from the stack and prints the character with the corresponding ASCI code
  * `CR`: prints a new line (for nice formating)
  * `STR`: converts the argument into a string (needs to work for all types)
  * `CONCAT2` and `CONCAT3` concatenates 2 or 3 strings from the stack (errors if arguments not strings)

3. Write 10 complete test files `t1.4TH` to `t10.4TH` (replace existing `t1.4TH`) and 10 corresponding output files `t1.out` to `t10.out`. Your code, when executing the `*.4TH` file should produce the exact output in `*.out` file. Write the output files by hand to test the correctness of the code. These are so called "functional tests" (as opposed to the smaller unit tests)

## how to run the code

## situations encountered.

1. GHC Version Compatibility & Cross-Platform Build Issues
Initially, when I tried building the given project on macOS (aarch64-darwin), I couldn't find a compatible GHC version. Therefore, I had to switch to a different operating system that the project supports in order to proceed.
2. Lazy Evaluation Complexity
Grasping Haskell's lazy evaluation model was challenging, especially since precise control over execution order and stack operations is essential in a Forth interpreter.
