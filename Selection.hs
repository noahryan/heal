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
import GeneticOperators(first, empty, count, rest)
import Control.Monad
import Data.Function
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr


newtype Min = Min { unMin :: Double } deriving Eq

instance Show Min where
  show (Min d) = show d
instance Ord Min where
  compare (Min d) (Min d') 
    | d == d' = EQ
    | d >= d' = LT
    | otherwise = GT

roulette pop = do
  let sumfit = F.sum $ fmap snd pop
  ds <- Tr.forM pop $ const (nextDouble sumfit)
  return $ fmap (select pop) ds

select v !d | empty v = error $ "No selection, fitness left is: " ++ show d
            | remaining <= 0.00001 = i
            | otherwise = select (rest v) remaining where
              (i, f) = first v
              remaining = d - f

tournament pop = do
  pop' <- S.replicateM (count pop) $ selectPlayers pop >>= compete
  return pop'
selectPlayers p = do
  i <- nextInt $ count p
  i' <- nextInt $ count p
  return $ (S.index p i, S.index p i')
compete (a, a') = do
  b <- test 0.75
  return $ fst $ maxBy snd a a'
maxBy f a a' = if f a > f a' then a else a'
    

--Best individual
best pop = F.maximumBy (\(_, a) (_, a') -> compare a a') pop
bestInd pop = fst $ best pop
bestFit pop = snd $ best pop

elitism gen pop = do
  pop' <- gen pop
  let oldbest@(_, f) = best pop
  let f' = bestFit pop'
  let pop'' = if f' < f && oldbest `F.notElem` pop' then oldbest S.<| rest pop' else pop'
  return pop''
