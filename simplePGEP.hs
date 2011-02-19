module Main(
  main
) where

import SymReg
import PGEP
import EAMonad
import EA
import Selection
import GeneticOperators(point)
import Operators
import Control.Monad.State

exps = 10
ps = 1000
is = 100
gens = 500
pm = 0.02
pr = 0.02
pc1 = 0.7
pc2 = 0.7
maxFit = 100000.0

ops = [plus, minus, mult, divide]
terms = [var] ++ constants [1, 2, 3, 5, 7]
syms = ops ++ terms

f x = (3*((x+1)^3)) + (2*((x+1)^2)) + (x+1)
--f x = (x^3) - (0.3*x^2) - (0.4*x) - (0.6)
--f x = x ^ 4 + x^3 + x^2 + x
--f x = x^2 + 3*x + 1
testcases = uniformCases (-10, 10) 21 f
eval = resError testcases maxFit

scale p = do
  scl <- getEnv
  let genbest = bestFit p
  let scaler = unMin scl
  let p' = fmap (\(a, (Min d)) -> (a, (scaler / (scaler+d)))) p
  putEnv $ if scl > genbest then scl else genbest
  return $! p'

run = ga (mkSymPop ps is (mkSymbols syms))
         (evaluate (eval . pgepEval))
         (\p-> scale p >>= roulette)
         (pgeprecomb pm pr pc1 pc2)
         True
         (maxGens gens)

main = do
  result <- experiment exps ""
  writeFile "ptestmany" result

experiment 0 l = return l
experiment times result = do
  (p, e, l, g) <- runEAIO run (Min maxFit)
  --let i = unfitness $ best p
  --let f = fitness $ best p
  let result' = result ++ l -- ++ "\n" ++ show (fmap point i) ++ " with fitness: " ++ show f
  experiment (times-1) result'
