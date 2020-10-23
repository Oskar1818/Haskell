-- Authors: Valter Miari, Clara Josefsson, Oskar Sturebrand
-- Lab group: 59
-- Date:

import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp
  deriving Eq -- (1)

--------------------------------------------------------------------------------
-- * A1
data Expr = Const Int
          | Expo Int
          | Op Expr BinOp Expr
            deriving Eq -- (1)

--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr e@(Const n)        = (eval 1 e) == n
prop_Expr (Expo n)           = n >= 0
prop_Expr e@(Op e1 AddOp e2) = prop_Expr e1 && prop_Expr e2
prop_Expr e@(Op e1 MulOp e2) = prop_Expr e1 && prop_Expr e2

--------------------------------------------------------------------------------
-- * A3
showExpr :: Expr -> String
showExpr e = case e of
  Const n        -> show n
  Expo 0         -> "1"
  Expo 1         -> "x"
  Expo n         -> "x^" ++ show n
  Op e1 AddOp e2 -> showExpr e1 ++ " + " ++ showExpr e2
  Op e1 MulOp e2 -> showMul  e1 ++ " * " ++ showMul  e2
  where
    showMul e @(Op e1 AddOp e2) = "(" ++ showExpr e ++ ")"
    showMul e                  = showExpr e

instance Show Expr where -- (1) -- Do (1) to show as (Const 1)
   show = showExpr

--------------------------------------------------------------------------------
-- * A4
genExpo :: Gen Expr
genExpo = do
  i <- arbitrary
  return (Expo (abs i))

genConst :: Gen Expr
genConst = do
  n <- arbitrary
  return $ Const n

genBinExpr :: Int -> Gen Expr
genBinExpr s = do
  o <- elements [AddOp, MulOp]
  let s' = div s 2
  l <- genExpr s'
  r <- genExpr s'
  return (Op l o r)

-- sample (arbitrary :: Gen Expr)
instance Arbitrary Expr where
    arbitrary = sized genExpr

genExpr :: Int -> Gen Expr
genExpr s = frequency [(1, genExpo), (1, genConst), (s, genBinExpr s)]

--------------------------------------------------------------------------------
-- * A5
eval :: Int -> Expr -> Int
eval _ (Const n) = 1 * n -- 3 == 3 * (x ^ 0)
eval x (Expo n)  = x ^ n
eval x (Op e1 AddOp e2) = (eval x e1) + (eval x e2)
eval x (Op e1 MulOp e2) = (eval x e1) * (eval x e2)

--------------------------------------------------------------------------------
-- * A6
-- Takes any expression and returns polynomials in the standard form
exprToPoly :: Expr -> Poly
exprToPoly e = case e of
  Const n -> fromList [n]
  Expo n -> fromList $ 1 : replicate n 0
  (Op e1 AddOp e2) -> (exprToPoly e1) + (exprToPoly e2)
  (Op e1 MulOp e2) -> (exprToPoly e1) * (exprToPoly e2)

ex1 = Op (Const 1) MulOp (Op (Op (Expo 1) MulOp (Op (Const 1) AddOp (Expo 2))) AddOp (Const 2))
ex3 = Op (Op (Const 1) AddOp (Const 2)) MulOp (Const 3)

prop_exprToPoly :: Int -> Expr -> Bool
prop_exprToPoly n e = eval n e == evalPoly n (exprToPoly e)

--------------------------------------------------------------------------------
-- * A7
polyToExpr :: Poly -> Expr
polyToExpr = polyToExpr' . toList

polyToExpr' :: [Int] -> Expr
polyToExpr' [] = Const 0
polyToExpr' [n] = Const n
polyToExpr' (0:xs) = polyToExpr' xs -- i.e. [0, 1, 1] == [1, 1]
--polyToExpr' (1:xs) = add (Expo (length xs)) (polyToExpr' xs) -- i.e. [1, 1, 1] = first 1 represents x to the power 2 (2 being the number of digits remains)
polyToExpr' (n:xs) = add (mul (Const n) (Expo (length xs))) (polyToExpr' xs) -- i.e. [3, 1 ,1]

add (Const 0) e            = e
add e         (Const 0)    = e
add (Expo 0)  e            = Op (Const 1) AddOp e
add e         (Expo 0)     = Op e AddOp (Const 1)
add (Const n) (Const m)    = Const (n+m)
add e1        e2           = Op e1 AddOp e2

mul (Const 0) e            =    (Const 0)
mul e         (Const 0)    =    (Const 0)
mul (Const 1) e            =    e
mul e         (Const 1)    =    e
mul (Expo 0)  e            =    e
mul e         (Expo 0)     =    e
mul (Const n) (Const m)    =    Const (n*m)
mul e1        e2           =    Op e1 MulOp e2

prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr n p = evalPoly n p == eval n (polyToExpr p)

--------------------------------------------------------------------------------
-- * A8
simplify :: Expr -> Expr
simplify = polyToExpr . exprToPoly
--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property
prop_noJunk :: Expr -> Bool
prop_noJunk = noJunk . simplify

noJunk :: Expr -> Bool

noJunk      (Expo 0)                         = False

noJunk (Op  (Const _)  _      (Const _))     = False
noJunk (Op  (Expo 0)   _      e)             = False
noJunk (Op  e          _      (Expo 0) )     = False
noJunk (Op  (Const 0)  _      e)             = False
noJunk (Op  e          _      (Const 0))     = False
noJunk (Op  (Const 1)  MulOp  e)             = False
noJunk (Op  e          MulOp  (Const 1) )    = False
noJunk (Op  e1         _  e2)                = noJunk e1 && noJunk e2

noJunk _                                     = True















--------------------------------------------------------------------------------
