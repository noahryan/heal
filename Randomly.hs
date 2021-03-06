{-# LANGUAGE BangPatterns #-}
module Randomly (
  test,
  might,
  mightM,
  nextDouble,
  nextInt,
  nextBool,
  selectFrom,
  shuffle,
  pairify
) where

import EAMonad
import System.Random.Mersenne.Pure64
import Control.Monad.Mersenne.Random

nextDouble :: Double -> EAMonad Double e
nextDouble d = do
  x <- randomly getDouble 
  return $! x*d

nextInt :: Int -> EAMonad Int e
nextInt d = do
  x <- randomly getInt 
  return $! x `mod` d

nextBool :: EAMonad Bool e
nextBool = randomly getBool

test :: Double -> EAMonad Bool e
test p = do  
  x <- nextDouble 1 
  return $! p > x

might p f a = do
  b <- test p
  return $ if b then f a else a

mightM p f a = do
  b <- test p
  if b then f a else return $ a 

selectFrom :: [a] -> EAMonad a e
selectFrom (a:as) = fairSelect 2.0 a as where
  fairSelect _ a [] = return a
  fairSelect n a (a':as) = do
    b <- test (1.0/n)
    let n' = n+1.0
    if b then fairSelect n' a' as else fairSelect n' a as

pairify :: [a] -> [(a,a)]
pairify [] = []
pairify (_:[]) = []
pairify (x:y:xs) = (x,y):pairify xs

shuffle :: [a] -> EAMonad [a] e
shuffle [] = return []
shuffle x = do 
  val <- nextInt $ length x
  rest <- shuffle $ removeNth x val
  return $ (x !! val):rest

removeNth :: [a] -> Int -> [a]
removeNth [] _ = []
removeNth (_:xs) 0 = xs
removeNth (x:xs) n = x:removeNth xs (n-1)
