{-# LANGUAGE BangPatterns #-}
module Selection (
  roulette,
  tournament,
  elitism,
  best,
  bestInd,
  bestFit,
  Min(..),
  Max(..)
) where

import EAMonad
import GeneticOperators(first, empty, count, rest)
import Randomly(nextDouble, nextInt, test)
import Control.Monad
import qualified Data.Traversable as Tr
import qualified Data.Foldable as F
import qualified Data.Sequence as S


newtype Min = Min { unMin :: Double } deriving (Show, Eq)
instance Ord Min where
  compare (Min d) (Min d') 
    | d == d' = EQ
    | d >= d' = LT
    | otherwise = GT

newtype Max = Max { unMax :: Double } deriving (Show, Eq, Ord)

--can be made more generic
roulette pop = do
  let sumfit = F.sum $ fmap (unMax . snd) pop
  ds <- Tr.forM pop $ const (nextDouble sumfit)
  return $! fmap (select pop) ds

select v !d | empty v = error $ "No selection, fitness left is: " ++ show d
            | remaining <= 0.00001 = i
            | otherwise = select (rest v) remaining where
              (i, Max f) = first v
              remaining = d - f

tournament pop = do
  pop' <- S.replicateM (count pop) $ selectPlayers pop >>= compete
  return pop'
selectPlayers p = do
  is <- S.replicateM 2 $ nextInt $ count p
  return $ fmap (S.index p) is
compete tourn = do
  b <- test 0.75
  return $! if b then bestInd tourn else bestInd tourn

--Best individual
best pop = F.maximum pop
bestInd pop = fst $ best pop
bestFit pop = snd $ best pop

elitism gen pop = do
  pop' <- gen pop
  let (oldbest, f) = best pop
  let (_, f') = best pop'
  let pop' = if f' < f && oldbest `F.notElem` pop' then oldbest S.<| (S.drop 1 pop') else pop'
  return pop
