module Main(
  main
) where

import RGEP
import SymReg
import EAMonad

ops = [plus, minus, mult, divide]
terms = [var] ++ constants [1, 2, 3, 5, 7]
f x = 2*x^3 + x^2 + 5*x + 12
testcases = uniformCases (-10, 10) 21 f
eval = resError testcases 100000
main = do
  (p, e, l, g) <- runEAIO (rgep 100 100 ops terms 0.02 0.02 0.7 0.7 eval 200) ()
  putStrLn l
