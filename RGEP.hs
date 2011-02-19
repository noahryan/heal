module RGEP(
  rgepEval,
  cdns2symbols,
  codonLength,
  mkCodonPop,
  rgep,
  rgeprecomb,
  cdn2int
) where

import Operators(OP)
import Postfix(postfix)
import EA--(ga, evaluate, maxGens)
import EAMonad
import Selection--(tournament, elitism, bestInd)
import Recombine--(mutation, rotation, crossover)
import Randomly
import qualified GeneticOperators as G
import Data.List
import Control.Monad
import Prelude 
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr

rgepEval = postfix

rgepmut pm pop = Tr.mapM (mutind pm) pop where
  mutind pm ind = do
    let len = G.count ind - 1
    indices <- F.foldlM (\is i -> might pm (i:) is) [] [0..len] 
    return $! foldl' (\ind' i -> S.adjust not i ind') ind indices

rgeprecomb cs pm pr pc1 pc2 pop = 
  rgepmut pm pop >>= rotation cs pr >>= 
  crossover 1 pc1 >>= crossover 2 pc2

rgep ps is ops terms pm pr pc1 pc2 eval gens = let cs = codonLength ops terms in
  ga (mkCodonPop ps is cs)
     (evaluate (eval . rgepEval . cdns2symbols ops terms))
     tournament
     (rgeprecomb cs pm pr pc1 pc2)
     True
     (maxGens gens)

cdns2symbols :: (G.Linear l) => [OP a] -> [OP a] -> l Bool -> l (OP a)
cdns2symbols ops terms cdns = trans cdns where
  trans cdns | G.isEmpty cdns = G.empty
             | otherwise = G.cons (syms !! cdn2int cdn) $ trans (G.drop cs cdns) where
               cdn = G.rest (G.take cs cdns)
               syms = if G.first cdns == True then terms' else ops'
  terms' = cycle terms
  ops' = cycle ops
  cs = codonLength ops terms

cdn2int bs = F.sum $ snd $ Tr.mapAccumL acc 1 bs where
  acc i b = (2*i, if b then i else 0)

-- ceil(log2(max(|ops|, |terms|)))
codonLength :: [a] -> [b] -> Int
codonLength ops terms = (1+) $ ceiling $ logBase 2 (fromIntegral $ max (length ops) (length terms))

codonPopulation ps is cs = S.replicate ps $ S.replicate (is*cs) True
mkCodonPop ps is cs = G.mutate (codonPopulation ps is cs)
