module Recombine(
  mutation,
  crossover,
  rotation,
) where

import EAMonad
import GeneticOperators 
import Data.Monoid
import Randomly(mightM)
import qualified Data.Traversable as Tr

mutation :: (Mutable a, Tr.Traversable f, Linear g) =>
  Double -> f (g a) -> EAMonad (f (g a)) e
mutation pm pop = Tr.mapM (Tr.mapM (mightM pm mutate)) pop

crossover :: (Pairable f, Tr.Traversable f, Crossable c) =>
  Int -> Double -> f c -> EAMonad (f c) e
crossover n pc pop = do
  paired <- pairup pop
  crossed <- Tr.mapM (mightM pc (cross n)) paired
  return $ unpair crossed

rotation :: (Rotatable r, Tr.Traversable f) => 
  Int -> Double -> f (r a) -> EAMonad (f (r a)) e
rotation i pr pop = Tr.mapM (mightM pr (rotate i)) pop
