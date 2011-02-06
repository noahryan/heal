module EA(
  ea,
  evaluate,
  loopM,
  maxGens
) where

import EAMonad
import Loci(Mutable, mutate)
import Selection
import qualified Data.Sequence as S
import qualified Data.Traversable as T

ea init eval select recombine gens opt elit pred = let
  gen p = incgen p >>= recordFitness opt >>= select >>= recombine 
  gen' p = eval p >>= if elit then elitism opt gen else gen in
    init >>= loopM pred gen' >>= eval
loopM :: (p -> EAMonad Bool e) -> (p -> EAMonad p e) -> p -> EAMonad p e
loopM pred f p = do
  b <- pred p
  if b then return p else f p >>= loopM pred f

maxGens :: Int -> p -> EAMonad Bool e
maxGens gens _ = do
  curgen <- getGens
  return $ gens <= curgen

evaluate eval p = do
  fits <- T.mapM eval p
  return $ S.zip p fits

recordFitness opt p = do
  record $ (++"\n") $ show $ bestFit opt p
  return p
