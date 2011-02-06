module SymReg (
  uniformCases,
  Expr(..),
  plus,
  minus,
  divide,
  var,
  dup,
  constants,
  constant,
  mult,
  expon,
  evalExpr,
  optimize,
  normalize,
  showp,
  multp,
  plusp
)
where

import PGEP
import Data.List(foldl1')

--safe divide
a // b | b /= 0 = a/b
  | otherwise = 0
pow a b = a ^^ truncate b

----ops
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
dup = OP { eats=1, leaves=2, applyOp=(\(a:as)->a:a:as), name="dup"}

uniformCases :: (Double, Double) -> Int -> (Double -> Double) -> [(Double, Double)]
uniformCases (start, end) num f = map makeCase [0..diff]
   where diff = end - start
         inc = diff / fromIntegral num
         makeCase x = (value, f value) where value = start + (x*inc)

data Expr = Con Double
          | Mul Expr Expr
          | Div Expr Expr
          | Sub Expr Expr
          | Add Expr Expr
          | Exp Expr Expr
          | Var deriving Eq

instance Show Expr where
  show (Con d) = show d
  show (Mul n m) = "(" ++ show n ++ "*" ++ show m ++ ")"
  show (Div n m) = "(" ++ show n ++ "/" ++ show m ++ ")"
  show (Sub n m) = "(" ++ show n ++ " - " ++ show m ++ ")"
  show (Add n m) = "(" ++ show n ++ " + " ++ show m ++ ")"
  show (Exp n m) = "(" ++ show n ++ "^" ++ show m ++ ")"
  show Var = "x"

exprmap :: (Expr -> Expr) -> Expr -> Expr
exprmap f (Add e e') = Add (f e) (f e')
exprmap f (Mul e e') = Mul (f e) (f e')
exprmap f (Exp e e') = Exp (f e) (f e')
exprmap f (Sub e e') = Sub (f e) (f e')
exprmap f (Div e e') = Div (f e) (f e')
exprmap f e          = e
mapopt = exprmap optimize'

normalize e = case e of
  Var -> [0, 1]
  Con n -> [n]
  Mul Var e1 -> 0:(normalize e1)
  Mul e1 Var -> 0:(normalize e1)
  Mul (Con n) e1 -> map (n*) (normalize e1)
  Mul e1 (Con n) -> map (n*) (normalize e1)
  Add e1 e2 -> plusp (normalize e1) (normalize e2)
  Sub e1 e2 -> plusp (normalize e1) (map (0-) (normalize e2))
  Mul e1 e2 -> multp (normalize e1) (normalize e2)
  Div e1 e2 -> error "can't normalize division"
multp [] vs = []
multp vs [] = []
multp (v:vs) (v':vs') = v*v' : (map (*v) vs') `plusp` (multp vs (v':vs'))
plusp [] vs = vs
plusp vs [] = vs
plusp (v:vs) (v':vs') = v+v':plusp vs vs'
showp p = showp' (reverse p) (length p - 1) where
  showp' [] _ = ""
  showp' (0:vs) i = showp' vs (i-1)
  showp' (1:0:[]) i = "x^" ++ show i
  showp' (1:vs) i = "x^" ++ show i ++ " + " ++ showp' vs (i-1)
  showp' (v:[]) i = (show v) ++ "x^" ++ show i
  showp' (v:vs) i = (show v) ++ "x^" ++ show i ++ " + " ++ showp' vs (i-1)

  
optimize e = untilSame $ iterate optimize' e
untilSame (a:a':as) = if a == a' then a else untilSame (a':as)
optimize' :: Expr -> Expr
optimize' e = case e of
   (Add (Con n) (Con m))      -> Con $ n+m
   (Add (Con 0) e)            -> optimize' e
   (Add e       (Con 0))      -> optimize' e
   (Mul (Con 1) e      )      -> optimize' e
   (Mul e       (Con 1))      -> optimize' e
   (Mul (Con n) (Con m))      -> Con $ n*m
   (Mul (Con 0) _      )      -> Con 0
   (Mul _       (Con 0))      -> Con 0
   (Div (Con n) (Con m))      -> Con $ n//m
   (Div _       (Con 0))      -> Con 1
   (Div e       (Con 1))      -> optimize' e
   (Sub (Con n) (Con m))      -> Con $ n-m
   (Sub e       (Con 0))      -> optimize' e
   (Exp (Con n) (Con m))      -> Con $ pow n m
   (Exp e       (Con 0))      -> Con 1
   (Exp e       (Con 1))      -> optimize' e
   (Exp e       (Exp e' e'')) -> mapopt $ Exp e (Mul e' e'')
   exp@(Sub e   e'     )      -> mapopt $ if e==e' then Con 0         else exp
   exp@(Div e   e'     )      -> mapopt $ if e==e' then Con 1         else exp
   exp@(Exp e   e'     )      -> mapopt $ if e==e' then Exp e (Con 2) else exp
   exp@(Mul e   e'     )      -> mapopt $ if e==e' then Exp e (Con 2) else exp
   exp@(Add e   e'     )      -> mapopt $ if e==e' then Mul (Con 2) e else exp
   con@(Con d          )      -> con
   var@(Var            )      -> var

evalExpr :: Expr -> Double -> Double
evalExpr e d = case e of
  (Add a a') -> evalExpr a d + evalExpr a' d
  (Sub a a') -> evalExpr a d - evalExpr a' d
  (Div a a') -> evalExpr a d // evalExpr a' d
  (Mul a a') -> evalExpr a d * evalExpr a' d
  (Exp a a') -> pow (evalExpr a d) (evalExpr a' d)
  Con n        -> n
  Var           -> d

exprToFunc e = case e of
  (Add a a') -> \x -> exprToFunc a x + exprToFunc a' x
  (Sub a a') -> \x -> exprToFunc a x - exprToFunc a' x
  (Div a a') -> \x -> exprToFunc a x // exprToFunc a' x
  (Mul a a') -> \x -> exprToFunc a x * exprToFunc a' x
  (Exp a a') -> \x -> exprToFunc a x `pow` exprToFunc a' x
  Con n       -> \x -> n
  Var          -> \x -> x
