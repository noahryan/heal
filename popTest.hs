{-# LANGUAGE BangPatterns #-}
module Main(
  main
) where

import RGEP
import Recombine
import GeneticOperators
import SymReg
import PGEP
import EA
import EAMonad
import Selection
import Control.Monad(sequence)
import qualified Data.Sequence as S

exps = 100
ps = 1000
cs = codonLength ops terms
is = 100

ops = [plus, minus, mult, divide]
terms = [var] ++ constants [2, 3, 7]
syms = terms ++ ops

f x = (3*((x+1)^3)) + (2*((x+1)^2)) + (x+1)
--f x = (x^3) - (0.3*x^2) - (0.4*x) - (0.6)
testcases = uniformCases (-10, 10) 21 f
eval Nothing = return 100000
eval (Just expr) = return $ if rawfit < 0.01 then 0 else rawfit where
  rawfit = sum $! map diff testcases 
  f = evalExpr expr
  diff (v, fv) = abs $! fv - f v
evalR = eval . rgepEval . (fmap point) --btrans ops terms
evalP = eval . pgepEval

flatpop ps is syms = S.replicateM ps $ S.replicateM is (mutate (syms!!0))
main = do
  result <- sequence $ replicate exps (evalEAIO (experiment (flatpop ps is (mkSymbols syms)) evalR) ())
  writeFile "rpop" $ concat result
  result <- sequence $ replicate exps (evalEAIO (experiment (mkSymPop ps is (mkSymbols syms)) evalP) ())
  writeFile "ppop" $ concat result

experiment init fitness = do
  p <- init 
  evaled <- evaluate fitness p
  let (_, f) = best Min evaled
  return $! show f ++ "\n"
