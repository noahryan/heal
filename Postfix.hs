module Postfix (
  postfix,
  dup,
  tuck,
  over,
  swap,
  rot,
  drp,
  nip
) where

import GeneticOperators
import Operators
import Prelude hiding (take)

-- Postfix evaluator for a sequence of operators
postfix ops = postfix' ops []
postfix' v ps | isEmpty v && null ps = Nothing
              | isEmpty v = Just $ head ps
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
drp' = Prelude.drop 1
nip' = drp' . swap' 
-- Operators for basic stack manipulation.
dup  = OP { eats=1, leaves=2, applyOp=dup',  name="dup"}
tuck = OP { eats=2, leaves=3, applyOp=tuck', name="tuck"}
over = OP { eats=2, leaves=3, applyOp=over', name="over"}
swap = OP { eats=2, leaves=2, applyOp=swap', name="swap"}
rot  = OP { eats=3, leaves=3, applyOp=rot',  name="rot"}
drp  = OP { eats=1, leaves=0, applyOp=drp',  name="drop"}
nip  = OP { eats=2, leaves=1, applyOp=nip',  name="nip"}
