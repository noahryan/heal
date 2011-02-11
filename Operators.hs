module Operators (
  OP(..),
  valid,
  validly,
  validate,
  validlyM
) where

import GeneticOperators
import qualified Data.Sequence as S

data OP a = OP { eats::Int, leaves::Int, applyOp::[a] -> [a], name::String }

instance Show (OP a) where
  show (OP _ _ _ name) = name
instance Eq (OP a) where
  (OP _ _  _ name) == (OP _ _ _ name') = name == name'

valid ind = check 1 $ S.reverse $ fmap (eats . point) ind where
  check n arrs | n == 0 = True
               | empty arrs = False
               | otherwise = check (n + (first arrs) - 1) (rest arrs)
pickvalid i i' = if valid i' then i' else i

validate p p' = S.zipWith pickvalid p p' 

validly f p = validate p (f p) 

validlyM f p = do
  p' <- f p
  return $ validate p p' 
