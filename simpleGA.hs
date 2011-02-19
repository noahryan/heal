module Main(
  main
) where

import GA
import EA
import EAMonad
import Selection
import Random
import Control.Monad.State
import qualified Data.Foldable as Fold

eval ind = return $ Fold.sum $ fmap toInt ind where
  toInt True = 1
  toInt False = 0

run = gaea 100 100 500 0.05 0.7 eval
--pop = evalState run (mkStdGen 33)

main = do
  (p, e, s) <- runEAIO run ()
  putStrLn s
  putStrLn $ show $ best Max $ p
