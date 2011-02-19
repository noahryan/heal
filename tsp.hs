module Main(
  main
) where

import RGEP
import SymReg
import PGEP
import EA
import EAMonad
import Postfix
import Selection
import Operators
import Data.List

exps = 1
ps = 500
is = 200
gens = 500
pm = 0.005
pr = 0.005
pc1 = 0.7
pc2 = 0.7
worst = 21*7

pairs = [(j, i) | j <- [0.0,5.0], i <- [0.0..5.0]]
swapped [] = []
swapped ((a, b):rest) = (b, a):swapped rest
cities = pairs `union` swapped pairs

catpath (a:b:rest) = (a `union` b) : rest
cat = OP { eats=2, leaves=1, applyOp=catpath, name="cat" }
revpath (a:rest) = reverse a:rest
rev = OP { eats=1, leaves=1, applyOp=revpath, name="rev" }
invert stack@([a]:rest) = stack
invert (as:rest) = (last as : (init (tail as) ++ [head as])) : rest
inv = OP { eats=1, leaves=1, applyOp=invert, name="inv" }

city point = OP { eats=0, leaves=1, applyOp=([point]:), name=show point}

ops = [cat, rev, inv]--, dup, drp, over, swap, rot, nip, tuck]
terms = map city $ delete (0, 0) cities 

pathLength (Just cits) = (7.0*(19.0 - fromIntegral (length cits))) + routelength cits' 0 where
  cits' = [(0.0, 0.0)] ++ cits
  routelength [] n = worst
  routelength [end] n = n + distance end (0, 0)
  routelength (a:b:rest) n = routelength (b:rest) $ n + distance a b
distance (a, b) (a', b') = sqrt ((a-a')^2 + (b-b')^2)
evalPath Nothing = return $ Min worst
evalPath path = return $ Min $ pathLength path

run = rgep ps is ops terms pm pr pc1 pc2 evalPath gens

main = do
  result <- experiment exps ""
  writeFile "traveling" result

experiment 0 l = return l
experiment times result = do
  (p, e, l, g) <- runEAIO run ()
  let (i, f) = best p
  let route = rgepEval (cdns2symbols ops terms i)
  let result' = result ++ l ++ "\n" ++ show route ++ "\n with fitness: " ++ show f ++ "\nand length " ++ show (fmap length route) ++ "\n"
  experiment (times-1) result'

