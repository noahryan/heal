{-# LANGUAGE BangPatterns #-}
module EAMonad (
  EAMonad,
  newEAGen,
  runEA,
  runEAIO,
  evalEA,
  evalEAIO,
  randomly,
  record,
  getGens,
  incgen
) where

import Data.Monoid
import Control.Applicative
import Control.Monad.State.Strict
import System.Random.Mersenne.Pure64
import Control.Monad.Mersenne.Random
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr

type EAMonad a e = StateT e (StateT Int (StateT String Rand)) a
  --runEA :: EAMonad a e -> PureMT -> (a, String, Int, e)
runEA m e g = (p, env, l, gen) where 
  (((p, env), gen), l) = evalRandom (runStateT (runStateT (runStateT m e) 0) "") g

evalEA m e g = let (p, _, _, _) = runEA m e g in p

runEAIO m e = do
  g <- newEAGen
  return $ runEA m e g
evalEAIO m e = do
  (p, _, _, _) <- runEAIO m e
  return p
newEAGen = newPureMT

record :: String -> EAMonad () e
record !s = lift $ lift $ modify (++s)
randomly r = lift $ lift $ lift r
incgen :: p -> EAMonad p e
incgen p = do
  lift $ modify succ
  return p
getGens :: EAMonad Int e
getGens = lift $ get
  

instance Functor Rand where
  fmap = liftM

instance Applicative Rand where
  pure = return
  (<*>) = ap
