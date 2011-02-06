module Recombine(
  mutation,
  crossover,
  rotation,
)
where

import EAMonad
import Loci
import Randomly(might, nextInt, nextBool, selectFrom)
import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr
import qualified Data.Vector as V

mutation :: (Mutable a, Tr.Traversable f, Tr.Traversable g) =>
  Double -> f (g a) -> EAMonad (f (g a)) e
mutation pm pop = Tr.mapM (Tr.mapM ((might pm mutate))) pop

crossover :: (Pairable f, Tr.Traversable f, Crossable c) =>
  Int -> Double -> f c -> EAMonad (f c) e
crossover n pc pop = do
  paired <- pairup pop
  crossed <- Tr.mapM (might pc (cross n)) paired
  return $ unpair crossed

crossList :: Int -> ([a], [a]) -> EAMonad ([a], [a]) e
crossList 0 cs = return cs
crossList n (as, bs) = do
  cp <- nextInt $ length as 
  crossed <- crossList (n-1) (take cp as ++ drop cp bs, take cp bs ++ drop cp as)
  return crossed

crossCrossable :: (Crossable a) => Int -> ([a], [a]) -> EAMonad ([a], [a]) e
crossCrossable 0 cs = return cs
crossCrossable n (as, bs) = do
  cp <- nextInt $ length as 
  (a,b) <- cross 1 (as!!cp, bs!!cp)
  let c1 = take (cp-1) as ++ [a] ++ drop (cp-1) bs
  let c2 = take (cp-1) bs ++ [b] ++ drop (cp-1) as
  crossed <- crossList (n-1) (c1, c2)
  return crossed

rotation :: (Rotatable r, Tr.Traversable f) => Double -> f (r a) -> EAMonad (f (r a)) e
rotation pr pop = Tr.mapM (might pr rotate) pop
rotateList :: [a] -> EAMonad [a] e
rotateList ind = do
  rp <- nextInt $ length ind 
  return $ drop rp ind ++ take rp ind

instance (Mutable a) => Mutable [a] where
  mutate as = Tr.mapM mutate as

instance Rotatable [] where
  rotate = rotateList

instance Crossable [a] where
  cross = crossList

instance Mutable Bool where
  mutate _ = nextBool

instance Mutable (PSet a) where
  mutate (PSet a as) = do
    a' <- selectFrom as
    return $ PSet a' as

instance (Mutable a) => Mutable (S.Seq a) where
  mutate = Tr.mapM mutate

instance Crossable (S.Seq a) where
  cross 0 s = return s
  cross i (a, b) = do
    let len = S.length a
    cp <- nextInt len
    let a' = S.take cp a S.>< S.drop cp b
    let b' = S.take cp b S.>< S.drop cp a
    result <- cross (i-1) (a', b')
    return result

instance Rotatable S.Seq where
  rotate s = do
    let len = S.length s
    rp <- nextInt len
    return $ S.drop rp s S.>< S.take rp s
