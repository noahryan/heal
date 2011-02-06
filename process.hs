import System
import System.IO
import Data.List.Split
import Data.List

gens = 500
trials = 10

process s = unlines $ map show $ map (/trials) sums where
  sums = foldr (zipWith (+)) (replicate gens 0) $ toNumbers gens lined
  lined = filter (not . null) $ lines s
toNumbers :: Int -> [String] -> [[Double]]
toNumbers n s = map (map read) $ map init $ splitEvery (n+1) s
  
main = do
  args <- getArgs
  let name = args !! 0
  contents <- readFile name 
  let result = process contents
  putStrLn result
  writeFile ("./data/" ++ name) result
