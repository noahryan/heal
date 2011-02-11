module Genetic(
  PSet(..),
  pointin,
  pointed,
  Pairable(pairup, unpair),
  Mutable(mutate),
  Crossable(cross),
  Rotatable(rotate),
)
where

import EAMonad
import Randomly(shuffle, pairify)
import Randomly(might, nextInt, nextBool, selectFrom)
import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr


class Pairable f where
  pairup :: f a -> EAMonad (f (a,a)) e
  unpair :: f (a,a) -> f a

instance Pairable [] where
  pairup as = do
    shuffled <- shuffle as
    return $! pairify shuffled
  unpair [] = []
  unpair ((a,b):as) = [a,b] ++ unpair as

instance Pairable S.Seq where
  pairup s = return $! S.zip firsthalf secondhalf where
    firsthalf = S.take half s
    secondhalf = S.drop half s
    half = S.length s `div` 2
  unpair s = fmap fst s S.>< fmap snd s

class Mutable r where
  mutate :: r -> EAMonad r e

class Crossable c where
  cross :: Int -> (c, c) -> EAMonad (c, c) e

class Rotatable r where
  rotate :: r a -> EAMonad (r a) e

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
rotateList :: [a] -> EAMonad [a] e
rotateList ind = do
  rp <- nextInt $ length ind 
  return $! drop rp ind ++ take rp ind

data PSet a = PSet { point::a, unpoint::[a] } deriving Show
pointin i = PSet i [i]
pointed i is = PSet i is

instance Functor PSet where
  fmap f (PSet i is) = PSet (f i) (map f is)

instance (Eq a) => Eq (PSet a) where
  (PSet a as) == (PSet a' as') = a == a' && as == as'

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
    return $! PSet a' as

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
    return $! S.drop rp s S.>< S.take rp s

