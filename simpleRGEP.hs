module Main(
  main
) where

import RGEP
import SymReg
import EA
import EAMonad
import Postfix

exps = 10
ps = 1000
is = 100
gens = 500
pm = 0.002
pr = 0.02
pc1 = 0.7
pc2 = 0.7
maxFit = 100000.0

ops = [plus, minus, mult, divide]
terms = [var] ++ constants [1..30]
--terms = [var] ++ constants [1, 2, 3, 5, 7]

f x = (3*((x+1)^3)) + (2*((x+1)^2)) + (x+1)
--f x = (x^3) - (0.3*x^2) - (0.4*x) - (0.6)
--f x = x ^ 4 + x^3 + x^2 + x
--f x = x^2 + 3*x + 1
testcases = uniformCases (-10, 10) 21 f
eval = resError testcases maxFit

run = rgep ps is ops terms pm pr pc1 pc2 eval gens

main = do
  result <- experiment exps ""
  writeFile "rtestmany" result

experiment 0 l = return l
experiment times result = do
  (p, e, l, g) <- runEAIO run ()
  --let (i, f) = best p
  --let ind = (drawAsTree $ fromJust $ postfix (cnds2symbols ops terms i)) ++ " with fitness: " ++ show f
  --let ind = show (cnds2symbols ops terms i) ++ " with fitness: " ++ show f
  experiment (times-1) $ result ++ l -- ++ ind
