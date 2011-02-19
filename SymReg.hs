module SymReg (
  uniformCases,
  Expr(..),
  evalExpr,
  drawAsTree,
  plus,
  minus,
  divide,
  var,
  constants,
  constant,
  mult,
  expon,
  resError
  --expx,
  --logx,
  --cosx,
  --sinx
) where

import Operators
import Selection
import Data.Tree
import Data.List(foldl1')

mkExpr :: (Expr -> Expr -> Expr) -> [Expr] -> Expr
mkExpr op (a:a':_) = op a a'
mkExpr op _ = error $ "Not enough arguments"
wrap op (a:a':as) = op a a' : as
binop nam op = OP {eats=2, leaves=1, applyOp=wrap op, name=nam}
plus   = binop "+" Add
minus  = binop "-" Sub
mult   = binop "*" Mul
divide = binop "/" Div
expon  = binop "^" Exp
var = OP {eats=0, leaves=1, applyOp=(Var:), name="x"}
constant d = OP {eats=0, leaves=1, applyOp=((Con d):), name=show d}
constants = map constant

uniformCases :: (Double, Double) -> Int -> (Double -> Double) -> [(Double, Double)]
uniformCases (start, end) num f = map makeCase [0..diff]
   where diff = end - start
         inc = diff / fromIntegral num
         makeCase x = (value, f value) where value = start + (x*inc)

resError _ worst Nothing = return $ Min worst
resError cases _ (Just expr) = return $ Min $ eval cases (evalExpr expr)
eval cases f = sum $ map diff cases where
  diff (v, fv) = abs $ fv - f v

data Expr = Con Double
          | Mul Expr Expr
          | Div Expr Expr
          | Sub Expr Expr
          | Add Expr Expr
          | Exp Expr Expr
          | Var deriving Eq

showop e name e' = "(" ++ show e ++ name ++ show e' ++ ")"
instance Show Expr where
  show (Con d) = show d
  show (Mul n m) = showop n "*" m
  show (Div n m) = showop n "/" m
  show (Sub n m) = showop n "-" m
  show (Add n m) = showop n "+" m
  show (Exp n m) = showop n "*" m
  show Var = "x"

--safe divide
a // b | b /= 0 = a/b
  | otherwise = 0
pow a b = a ^^ truncate b

evalExpr :: Expr -> Double -> Double
evalExpr e d = case e of
  (Add a a') -> evalExpr a d + evalExpr a' d
  (Sub a a') -> evalExpr a d - evalExpr a' d
  (Div a a') -> evalExpr a d // evalExpr a' d
  (Mul a a') -> evalExpr a d * evalExpr a' d
  (Exp a a') -> pow (evalExpr a d) (evalExpr a' d)
  Con n      -> n
  Var        -> d

drawAsTree = drawTree . toTree where
  toTree (Add e e') = Node "+" [toTree e, toTree e']
  toTree (Sub e e') = Node "-" [toTree e, toTree e']
  toTree (Div e e') = Node "/" [toTree e, toTree e']
  toTree (Mul e e') = Node "*" [toTree e, toTree e']
  toTree (Exp e e') = Node "^" [toTree e, toTree e']
  toTree (Con c)    = Node (show c) []
  toTree Var        = Node "x" []
