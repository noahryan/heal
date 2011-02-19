{-# LANGUAGE BangPatterns #-}
module Selection (
  roulette,
  tournament,
  elitism,
  best,
  bestInd,
  bestFit,
  Min(..),
) where

import EAMonad
import Randomly(nextDouble, nextInt, test)
import GeneticOperators(Linear(..))
import Control.Monad
import Data.Function
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr


newtype Min = Min { unMin :: Double } deriving Eq

instance Show Min where
  show (Min d) = show d
instance Ord Min where
  compare (Min d) (Min d') 
    | d == d' = EQ
    | d >= d' = LT
    | otherwise = GT

-- Roulette Wheel Selection.
roulette :: (Linear p) => p (a, Double) -> EAMonad (p a) e
roulette pop = do
  let sumfit = F.sum $ fmap snd pop
  ds <- Tr.forM pop $ const (nextDouble sumfit)
  return $ fmap (select pop) ds
select v !d | isEmpty v = error $ "No selection, fitness remaining: " ++ show d
            | remaining <= 0.00001 = i
            | otherwise = select (rest v) remaining where
              (i, f) = first v
              remaining = d - f

-- Tournament Selection.
tournament :: (Linear p, Ord b) => p (a, b) -> EAMonad (p a) e
tournament pop = do
  pop' <- Tr.forM pop $ const $ selectPlayers pop >>= compete
  return pop'
selectPlayers p = do
  i <- nextInt $ count p
  i' <- nextInt $ count p
  return $ (index p i, index p i')
compete (a, a') = do
  b <- test 0.75
  return $! if b then larger else smaller where
    larger = fst $ maxBy snd a a'
    smaller = fst $ minBy snd a a'
maxBy f a a' = if f a > f a' then a else a'
minBy f a a' = if f a < f a' then a else a'
    

-- Best individual.
best :: (Linear p, Ord b) => p (a, b) -> (a, b)
best pop = F.maximumBy (\(_, a) (_, a') -> compare a a') pop
bestInd :: (Linear p, Ord b) => p (a, b) -> a
bestInd pop = fst $ best pop
bestFit :: (Linear p, Ord b) => p (a, b) -> b
bestFit pop = snd $ best pop

-- Elitism.
elitism :: (Linear p, Ord b, Eq a) =>
  (p (a, b) -> EAMonad (p (a, b)) e) -> -- generation
  p (a, b) ->                           -- population
  EAMonad (p (a, b)) e
elitism gen pop = do
  pop' <- gen pop
  let oldbest@(_, f) = best pop
  let f' = bestFit pop'
  let pop'' = if f' < f && oldbest `F.notElem` pop' then oldbest `cons` rest pop' else pop'
  return pop''
