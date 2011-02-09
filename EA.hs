module EA(
  ea,
  ga,
  evaluate,
  maxGens,
  minFitness,
  maxFitness
) where

import EAMonad
import Selection
import GeneticOperators
import qualified Data.Traversable as T

ea init gen pred = init >>= loopM pred gen' where
  gen' p = incgen p >>= recordFitness >>= gen
loopM pred f p = do
  b <- pred p
  if b then return p else f p >>= loopM pred f

ga init eval select recombine elit pred = let
  gen p = incgen p >>= recordFitness >>= select >>= recombine >>= eval
  gen' = if elit then elitism gen else gen in
    ea (init >>= eval) gen' pred

maxGens gens p = do
  curgen <- getGens p
  return $! gens <= curgen

minFitness minfit p = return $! minfit >= bestFit p
maxFitness maxfit p = return $! maxfit <= bestFit p

evaluate eval p = T.mapM eval' p where
  eval' i = do
    fit <- eval i
    return (i, fit)

recordFitness p = do
  record $! (show $ bestFit p) ++ "\n" 
  return p
