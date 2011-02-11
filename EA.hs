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

ea :: EAMonad a e -> (a -> EAMonad a e) -> (a -> EAMonad Bool e) -> EAMonad a e
ea init gen pred = init >>= loopM pred gen' where
  gen' p = incgen >> gen p
loopM pred f p = do
  b <- pred p
  if b then return p else f p >>= loopM pred f

ga init eval select recombine elit pred = let
  gen p = recordFitness p >>= select >>= recombine >>= eval
  gen' = if elit then elitism gen else gen in
    ea (init >>= eval) gen' pred

maxGens gens p = do
  curgen <- getGens
  return $ gens <= curgen

minFitness minfit p = return $ minfit >= bestFit p
maxFitness maxfit p = return $ maxfit <= bestFit p

evaluate eval p = T.mapM eval' p where
  eval' i = do
    fit <- eval i
    return $ (i, fit)

recordFitness p = do
  let fit = bestFit p
  record $! "BestFit:: " ++ show fit ++ "\n"
  return (fit `seq` p)
