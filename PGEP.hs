{-# LANGUAGE BangPatterns #-}
module PGEP(
  pgepEval,
  mkSymPop,
  pgep,
  mkSymbols,
  pgeprecomb
) where

import EA
import EAMonad
import Operators
import Randomly(mightM)
import Postfix(postfix)
import GeneticOperators
import Selection(elitism, roulette)
import Recombine(mutation, rotation, crossover)
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr


pgepEval = postfix . S.reverse . uncode . (fmap point)

uncode ops = uncode' ops 1 where
  uncode' _ 0 = S.empty
  uncode' ops n = cons op $ uncode' (rest ops) (n - 1 + eats op) where
    op = first ops
    
pgeprecomb pm pr pc1 pc2 pop = 
  validlyM (mutation pm) pop >>= validlyM (rotation 1 pr) >>= 
  (pcross 1 pc1) >>= (pcross 2 pc2)
pcross n pc pop = do
  paired <- pairup pop
  crossed <- Tr.mapM (mightM pc (cross n)) $ paired
  return $ validate pop $ unpair crossed

pgep ps is gens pm pr pc1 pc2 eval syms = 
  ga (mkSymPop ps is syms)
     (evaluate (eval . pgepEval))
     roulette
     (pgeprecomb pm pr pc1 pc2)
     True
     (maxGens gens)

mkSymbols ops = [pointed op ops | op <- ops]
  
mkSymPop ps is syms = loop S.empty where
  ind = S.replicate is (syms!!0)
  loop pop = if S.length pop == ps then return pop else do
    ind' <- mutate ind
    if not $ valid ind' then loop pop else loop (ind' S.<| pop)
