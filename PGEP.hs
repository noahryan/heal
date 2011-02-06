{-# LANGUAGE BangPatterns #-}
module PGEP(
  OP(..),
  pgepEval,
  postfix,
  --uncode,
  mkSymPop,
  pgepea,
  mkSymbols,
  pgeprecomb,
  valid
)
where

import EA
import Loci
import EAMonad
import Randomly(might)
import Selection(elitism, roulette, OPT(Max))
import Recombine(mutation, rotation, crossover)
import Data.Traversable
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr

type PChrom a = S.Seq (PSet a)
type PPop a = S.Seq (PChrom a)

data OP a = OP { eats::Int, leaves::Int, applyOp::[a] -> [a], name::String }
stkeffect op = eats op - leaves op
instance Show (OP a) where
  show (OP _ _ _ name) = name
instance Eq (OP a) where
  (OP _ _  _ name) == (OP _ _ _ name') = name == name'


--uncode :: V.Vector (OP a) -> V.Vector (OP a)
--uncode ops = V.slice 0 (treelength 0 0 0 ops) ops where 
--  treelength !need !have !l ops | need < have || V.null ops = l
--    | otherwise = treelength (need + eats op) (have + leaves op) (l+1) (V.tail ops) where
--      op = V.head ops

postfix :: S.Seq (OP a) -> Maybe a
postfix ops = postfix' ops []
postfix' :: S.Seq (OP a) -> [a] -> Maybe a
postfix' v ps | S.null v && null ps = Nothing
              | S.null v = Just $ head ps
              | otherwise = let op = S.index v 0 in
                postfix' (S.drop 1 v) (updateStack (eats op) (applyOp op) ps)
updateStack n f ps = if n `overflows` ps then ps else f ps
overflows 0 _ = False
overflows _ [] = True
overflows !n (_:as) = overflows (n-1) as


pgepEval :: S.Seq (PSet (OP a)) -> Maybe a
pgepEval = postfix . S.reverse . (fmap point)
--pgepEval = postfix . V.reverse . uncode . (V.map point)

pgeprecomb pm pr pc1 pc2 pop = 
  validlyM (mutation pm) pop >>= validlyM (rotation pr) >>= 
  (pcross 1 pc1) >>= (pcross 2 pc2)
pcross n pc pop = do
  paired <- pairup pop
  crossed <- Tr.mapM (might pc (cross n)) $ paired
  return $ validate pop $ unpair crossed

pgepea ps is gens pm pr pc1 pc2 eval syms = 
  ea (mkSymPop ps is syms)
     (evaluate eval)
     roulette
     (pgeprecomb pm pr pc1 pc2)
     gens
     Max
     True

mkSymbols ops = [pointed op ops | op <- ops]
  
valid ind = check 1 $ fmap (eats . point) ind where
  check !n arrs | n == 0 = True
               | S.null arrs = False
               | otherwise = check (n + (S.index arrs 0) - 1) (S.drop 1 arrs)
validly f p = validate p (f p) 
validlyM f p = do
  p' <- f p
  return $ validate p p' 
validate p p' = S.zipWith pickvalid p p' 
pickvalid i i' = if valid i' then i' else i

mkSymPop ps is syms = loop S.empty where
  ind = S.replicate is (syms!!0)
  loop pop = if S.length pop == ps then return pop else do
    ind' <- mutate ind
    if not $ valid ind' then loop pop else loop (ind' S.<| pop)
