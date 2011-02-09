module Postfix (
  postfix,
  dup,
  tuck,
  over,
  swap,
  rot,
  drp
) where

import GeneticOperators
import Operators

-- Postfix evaluator for a sequence of operators
postfix ops = postfix' ops []
postfix' v ps | empty v && null ps = Nothing
              | empty v = Just $ head ps
              | otherwise = let op = first v in
                postfix' (rest v) (updateStack (eats op) (applyOp op) ps)
updateStack n f ps = if n `overflows` ps then ps else f ps
overflows 0 _ = False
overflows _ [] = True
overflows n (_:as) = overflows (n-1) as

--helper functions
dup' l = (l!!0):l
tuck' (a:a':as) = a:a':a:as
over' (a:a':as) = a':a:a':as
rot' (a:a':a'':as) = a'':a:a':as
swap' = drp' . tuck'
drp' = drop 1
-- Operators for basic stack manipulation.
dup  = OP { eats=1, leaves=2, applyOp=dup',  name="dup"}
tuck = OP { eats=2, leaves=2, applyOp=tuck', name="tuck"}
over = OP { eats=2, leaves=3, applyOp=over', name="over"}
swap = OP { eats=2, leaves=3, applyOp=swap', name="swap"}
rot  = OP { eats=3, leaves=3, applyOp=rot',  name="rot"}
drp  = OP { eats=1, leaves=0, applyOp=drp',  name="drop"}
