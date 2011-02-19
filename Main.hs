module Main(
  main
) where

import SymReg
import RGEP
import PGEP
import EAMonad
import EA

--ps = 1000
ps = 100
cs = codonlength ops terms
is = 128
--gens = 500
gens = 10
pm = 0.01
pr = 0.01
pc1 = 0.35
pc2 = 0.35
opt = Min

ops = [plus, minus, mult, divide]
terms = [var] ++ constants [1, 2, 3, 5, 7]

--f x = x^2 + 3*x + 1
--f x = 3*(x+1)^3 +2*(x+1)^2 + (x+1)
f x = x^3 - 0.3*x^2 - 0.4*x - 0.6
testcases = uniformCases (-10, 10) 21 f
evalF Nothing = return 100000
evalF (Just expr) = return $ sum $ map (uncurry diff) testcases where
  f = evalExpr expr
  diff v fv = abs $ fv - f v
trans = btrans ops terms
eval = evalF . rgepEval . trans

run = setupEA $ rgepea ps is cs pm pr pc1 pc2 opt eval gens

main = do
  p <- evalEAIO run ()
  print $ "size is: " ++ show (ps*is*cs)
  let (i, f) = best opt p
  print $ rgepEval . trans i
  print $ "with fitness: " ++ show f
