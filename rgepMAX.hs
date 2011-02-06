module Main(
  main,
) where

import RGEP
import SymReg
import PGEP
import EA
import EAMonad
import Selection

exps = 1
ps = 200
cs = codonLength ops terms
is = 2^7 - 1
gens = 500
pm = 0.02
pr = 0.02
pc1 = 0.7
pc2 = 0.7
opt = Max

ops = [mult, plus]
terms = [constant 0.5]

evalF Nothing = return 0
evalF (Just expr) = return $ evalExpr expr undefined

run = rgepea ps is ops terms pm pr pc1 pc2 opt evalF gens

main = do
  result <- experiment exps ""
  writeFile "exprmax" result

experiment 0 l = return l
experiment times result = do
  (p, e, l, g) <- runEAIO run ()
  let (i, f) = best opt p
  let result' = result ++ l ++ "\n" ++ show (btrans ops terms i)
  experiment (times-1) result'

