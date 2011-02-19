import System
import System.IO
import Data.List.Split
import Data.List

gens = 500
trials = 10

fitness = "BestFit: "

process :: String -> String
process s = show $ stdev $ splitUp $ toNumbers $ getFits s where

toNumbers :: [String] -> [Double]
toNumbers s = map read s

splitUp :: [a] -> [a]
splitUp s = map last $ splitEvery gens s

getFits :: String -> [String]
getFits s = map (drop (length fitness)) $ filter (isPrefixOf fitness) $ lines s

stdev :: [Double] -> Double
stdev nums = sqrt $ (/len) $ sum $ map (\i -> (avg-i)^2) nums where
  len = fromIntegral $ length nums
  avg = (sum nums) / len
  
main = do
  args <- getArgs
  let name = args !! 0
  contents <- readFile name 
  let result = process contents
  putStrLn result
