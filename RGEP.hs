module RGEP(
  rgepEval,
  btrans,
  codonLength,
  mkCodonPop,
  rgep,
  rgeprecomb,
  cdn2int
)
where

import EA
import PGEP
import GeneticOperators(mutate)
import Randomly(might)
import EAMonad(EAMonad)
import Selection(roulette)
import EA(ea, evaluate, maxGens)
import Selection(tournament, elitism)
import Recombine(mutation, rotation, crossover)
import Postfix(postfix)
import Operators(OP)
import Data.Maybe(fromJust, isJust)
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Sequence as S

type Codon = S.Seq Bool
type RChrom = S.Seq Codon

rgepEval :: S.Seq (OP a) -> Maybe a
rgepEval = postfix

rgeprecomb pm pr pc1 pc2 pop = 
  mutation pm pop >>= rotation pr >>= 
  crossover 1 pc1 >>= crossover 2 pc2

rgep ps is ops terms pm pr pc1 pc2 eval gens = 
  ga (mkCodonPop ps is (codonLength ops terms))
     (evaluate (eval . rgepEval . btrans ops terms))
     tournament
     (rgeprecomb pm pr pc1 pc2)
     True
     (maxGens gens)

btrans :: [OP a] -> [OP a] -> RChrom -> S.Seq (OP a)
btrans ops terms cdns = fmap trans cdns where
  trans = codon2Op ops terms
--btrans ops terms cdns = V.map fromJust $ V.filter isJust $ V.map trans cdns where

codon2Op ops terms cnd = syms !! index where
  index = cdn2int (S.drop 1 cnd) `mod` (length syms) 
  syms = if cnd `S.index` 0 == True then ops else terms
--codon2Op :: [OP a] -> [OP a] -> Codon-> Maybe (OP a)
--codon2Op ops terms cnd = syms V.! (cdn2int cnd) where
--  k = codonLength ops terms
--  syms = V.fromList $ fillout ops k ++ fillout terms k

cdn2int bs = F.sum $! S.zipWith (\b i -> if b then i else 0) bs is where
  is = S.fromList [2^i | i <- [0..S.length bs-1]]
--V.ifoldl' (\ acc i v -> acc + if v then 2^i else 0) 0 bs

--fillout as k = (map (Just) filled) ++ nothings
--  where l = length as
--        s = 2 ^ k
--        m = s `div` l
--        nothings = replicate (s - (m * l)) Nothing
--        filled = concat (replicate m as)

--ceil(log2(max(|ops|, |terms|)))
codonLength :: [a] -> [b] -> Int
codonLength ops terms = (1+) $ ceiling $ logBase 2 (fromIntegral $ max (length ops) (length terms))

codonPopulation ps is cs = S.replicate ps $ S.replicate is $ S.replicate cs True
mkCodonPop ps is cs = mutate (codonPopulation ps is cs)
