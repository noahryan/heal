module Main(
  main,
  testcases
) where

import SymReg
import PGEP
import EAMonad
import EA
import Selection
import Loci(point)
import qualified Data.Foldable as F
import Control.Monad.State

exps = 10
ps = 1000
is = 100
gens = 1
pm = 0.02
pr = 0.02
pc1 = 0.7
pc2 = 0.7
opt = Max
maxFit = 100000.0

ops = [plus, minus, mult, divide]
terms = [var] ++ constants [1, 2, 3, 5, 7]
syms = ops ++ terms

--f x = (3*((x+1)^3)) + (2*((x+1)^2)) + (x+1)
f x = (x^3) - (0.3*x^2) - (0.4*x) - (0.6)
--f x = x ^ 4 + x^3 + x^2 + x
--f x = x^2 + 3*x + 1
testcases = uniformCases (-10, 10) 21 f
evalF Nothing = return maxFit
evalF (Just expr) = return $ sum $ map diff testcases where
  f = evalExpr expr
  diff (v, fv) = abs $ fv - f v
eval ind = evalF $ pgepEval ind

scale p = do
  scl <- get 
  let genbest = F.minimum (fmap snd p)
  let p' = fmap (\(a,d) -> (a, scl / (scl+d))) p
  put $ min scl genbest
  return p'

run = ea (mkSymPop ps is (mkSymbols syms))
         (evaluate eval)
         (\p-> scale p >>= roulette)
         (pgeprecomb pm pr pc1 pc2)
         gens
         Min
         True
         (maxGens gens)

main = do
  result <- experiment exps ""
  writeFile "ptest" result

experiment 0 l = return l
experiment times result = do
  (p, e, l, g) <- runEAIO run maxFit
  let (i, f) = best Min p
  let result' = result ++ l ++ "\n" ++ show (fmap point i) ++ " with fitness: " ++ show f
  experiment (times-1) result'
