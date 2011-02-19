module Main(main) where

import System
import System.IO
import Data.List
import Graphics.Rendering.Chart.Simple

pre = "BestInd: "
prelude = "module Main(main) where\nimport Graphics.Rendering.Chart.Simple\nrng=[-10.0..10.0]::[Double]\nmain = do\n"
process s = unlines $ map wrapF $ addIndex $ map dropPre $ filter (isPrefixOf pre) $ lines s

dropPre = drop (length pre)

addIndex ls = zip [1..length ls] ls

wrapF (i, l) = "  plotPNG (\"" ++ show i ++ ".png\" ) rng " ++ "(\\ x -> " ++ l ++ ")"

main = do
  args <- getArgs
  let name = args !! 0
  contents <- readFile name
  let fs = process contents
  writeFile ("data/" ++ name) $ prelude ++ fs
