module Main(
  main,
  testcases
) where

import RGEP
import SymReg
import PGEP
import EA
import EAMonad
import Selection

exps = 1
ps = 1000
cs = codonLength ops terms
is = 100
gens = 0
pm = 0.00
pr = 0.00
pc1 = 0.0
pc2 = 0.0
opt = Min

ops = [plus, minus, mult, divide]
terms = [var] ++ constants [1, 2, 3, 5, 7]

f x = (3*((x+1)^3)) + (2*((x+1)^2)) + (x+1)
--f x = (x^3) - (0.3*x^2) - (0.4*x) - (0.6)
--f x = x ^ 4 + x^3 + x^2 + x
--f x = x^2 + 3*x + 1
testcases = uniformCases (-10, 10) 21 f
evalF Nothing = return 100000
evalF (Just expr) = return $ if rawfit < 0.01 then 0 else rawfit where
  rawfit = sum $ map diff testcases 
  f = evalExpr expr
  diff (v, fv) = abs $ fv - f v

run = rgepea ps is ops terms pm pr pc1 pc2 opt evalF gens

main = do
  result <- experiment exps ""
  writeFile "rtest" result

experiment 0 l = return l
experiment times result = do
  (p, e, l, g) <- runEAIO run ()
  let (i, f) = best Min p
  let result' = result ++ l ++ "\n" ++ show (btrans ops terms i) ++ " with fitness: " ++ show f
  experiment (times-1) result'

