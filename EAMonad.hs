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
  getEnv,
  putEnv,
  incGen
) where

import Control.Applicative
import Control.Monad.State.Strict
import System.Random.Mersenne.Pure64
import Control.Monad.Mersenne.Random
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr

type EAMonad a e = StateT (e, String, Int) Rand a

runEA :: EAMonad a e -> e -> PureMT -> (a, e, String, Int)
runEA m e g = (p, env, l, gen) where 
  (p, (env, l, gen)) = evalRandom (runStateT m (e, "", 0)) g

evalEA :: EAMonad a e -> e -> PureMT -> a
evalEA m e g = let (p, _, _, _) = runEA m e g in p

runEAIO :: EAMonad a e -> e -> IO (a, e, String, Int)
runEAIO m e = do
  g <- newEAGen
  return $ runEA m e g

evalEAIO :: EAMonad a e -> e -> IO a
evalEAIO m e = do
  (p, _, _, _) <- runEAIO m e
  return p

newEAGen :: IO PureMT
newEAGen = newPureMT

getEnv :: EAMonad e e
getEnv = do
  (e, l, g) <- get
  return e

modifyEnv :: (e -> e) -> EAMonad () e
modifyEnv f = do
  (e, l, g) <- get
  put (f e, l, g)

putEnv :: e -> EAMonad () e
putEnv e = do
  (_, l, gens) <- get
  put (e, l, gens)

record :: String -> EAMonad () e
record !s = modify (\(e, l, gen) -> (e, l++s, gen))

randomly :: Rand a -> EAMonad a e
randomly r = lift r

incGen :: EAMonad () e
incGen = modify (\(e, l, gen) -> (e, l, succ gen))

getGens :: EAMonad Int e
getGens = do
  (_, _, gens) <- get
  return $! gens
