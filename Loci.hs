module Loci(
  PSet(..),
  pointin,
  pointed,
  Pairable(pairup, unpair),
  Mutable(mutate),
  Crossable(cross),
  Rotatable(rotate)
)
where

import EAMonad
import Randomly(shuffle, pairify)
import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr
import qualified Data.Foldable as F


class Pairable f where
  pairup :: f a -> EAMonad (f (a,a)) e
  unpair :: f (a,a) -> f a

instance Pairable [] where
  pairup as = do
    shuffled <- shuffle as
    return $ pairify shuffled
  unpair [] = []
  unpair ((a,b):as) = [a,b] ++ unpair as

flength l = F.foldl' (+) 0 $ fmap (const 1) l

instance Pairable S.Seq where
  pairup s = return $ S.zip firsthalf secondhalf where
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


data PSet a = PSet { point::a, unpoint::[a] } deriving Show
pointin i = PSet i [i]
pointed i is = PSet i is

instance Functor PSet where
  fmap f (PSet i is) = PSet (f i) (map f is)

instance (Eq a) => Eq (PSet a) where
  (PSet a as) == (PSet a' as') = a == a' && as == as'

