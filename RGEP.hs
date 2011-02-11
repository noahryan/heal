module RGEP(
  rgepEval,
  cnds2symbols,
  codonLength,
  mkCodonPop,
  rgep,
  rgeprecomb,
  cdn2int
)
where

import Operators(OP)
import Postfix(postfix)
import GeneticOperators--(mutate, first, rest, Linear)
import EA--(ga, evaluate, maxGens)
import EAMonad
import Selection--(tournament, elitism, bestInd)
import Recombine--(mutation, rotation, crossover)
import Randomly
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr

rgepEval = postfix

rgeprecomb pm pr pc1 pc2 pop = 
  Tr.mapM (Tr.mapM (Tr.mapM ((might pm mutate)))) pop >>= rotation pr >>= 
  crossover 1 pc1 >>= crossover 2 pc2

rgep ps is ops terms pm pr pc1 pc2 eval gens = 
  ga (mkCodonPop ps is (codonLength ops terms))
     (evaluate (eval . rgepEval . cnds2symbols ops terms))
     tournament
     --(\p->recordBest ops terms p >>= tournament)
     (rgeprecomb pm pr pc1 pc2)
     True
     (maxGens gens)
--recordBest ops terms p = do
--  let i = bestInd p
--  record $ ("BestInd: " ++) $ show $ rgepEval $ cnds2symbols ops terms i
--  record "\n"
--  return p

cnds2symbols ops terms cdns = fmap (codon2Op ops terms) cdns

codon2Op ops terms cnd = syms !! (cdn2int $ rest cnd) where
  syms = if first cnd == True then ops' else terms'
  ops' = cycle ops
  terms' = cycle terms

cdn2int bs = F.sum $! snd $! Tr.mapAccumL acc 1 bs where
  acc i b = (2*i, if b then i else 0)

--ceil(log2(max(|ops|, |terms|)))
codonLength :: [a] -> [b] -> Int
codonLength ops terms = (1+) $ ceiling $ logBase 2 (fromIntegral $ max (length ops) (length terms))

codonPopulation ps is cs = S.replicate ps $ S.replicate is $ S.replicate cs True
mkCodonPop ps is cs = mutate (codonPopulation ps is cs)
