{-# LANGUAGE BangPatterns #-}
module Selection (
  roulette,
  tournament,
  elitism,
  OPT(..),
  best,
  bestInd,
  bestFit
) where

import EAMonad
import Control.Monad
import Randomly
import Data.List(sortBy)
import Randomly(nextDouble, nextInt)
import qualified Data.Traversable as Tr
import qualified Data.Foldable as F
import qualified Data.Sequence as S


data OPT = Min | Max deriving (Eq, Show)

flipOpt Max = Min
flipOpt Min = Max

--Selection
roulette :: S.Seq (a, Double) -> EAMonad (S.Seq a) e
roulette pop = do
  let sumfit = F.sum $ fmap snd pop
  ds <- Tr.forM pop $ const (nextDouble sumfit)
  return $ fmap (select pop) ds

select v !d | S.null v = error $ "No selection, fitness left is: " ++ show d
            | rest <= 0.00001 = i
            | otherwise = select (S.drop 1 v) rest where
              (i, f) = v `S.index` 0
              rest = d - f

tournament :: OPT -> S.Seq (a, Double) -> EAMonad (S.Seq a) e
tournament opt pop = do
  pop' <- S.replicateM (S.length pop) $ selectPlayers pop >>= compete opt
  return pop'
selectPlayers p = do
  is <- S.replicateM 2 $ nextInt $ S.length p
  return $ fmap (S.index p) is
compete opt tourn = do
  b <- test 0.75
  return $ if b then bestInd opt tourn else bestInd (flipOpt opt) tourn

--Best individual
best Min pop = F.minimumBy (\(_,d) (_,d') -> compare d d') pop
best Max pop = F.maximumBy (\(_,d) (_,d') -> compare d d') pop
bestInd opt pop = fst $ best opt pop
bestFit opt pop = snd $ best opt pop

elitism opt gen pop = do
  sel <- gen pop
  let (a, d) = best opt pop
  let pop' = if a `F.notElem` sel then a S.<| (S.drop 1 sel) else sel
  return pop'
