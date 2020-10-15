-- Authors:
-- Date:

import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp
  deriving Eq -- (Show, Eq) -- (1)

--------------------------------------------------------------------------------
-- * A1
data Expr = Const Int
          | Expo Int
          | Op Expr BinOp Expr
            deriving Eq -- (Show, Eq) -- (1)

--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr e@(Const n)        = (eval 1 e) == n
prop_Expr (Expo n)           = n >= 0
prop_Expr e@(Op e1 AddOp e2) = e == e
prop_Expr e@(Op e1 MulOp e2) = e == e

--------------------------------------------------------------------------------
-- * A3
showExpr :: Expr -> String
showExpr e = case e of
  Const n        -> show n
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
genExpr :: Gen Expr
genExpr = do
  t <- elements [Expo, Const]
  i <- suchThat arbitrary (\i -> (i >= 0) && (i < 15))
  return (t i)

genBinExpr :: Gen Expr
genBinExpr = do
  o <- elements [AddOp, MulOp]
  l <- arbitrary
  r <- arbitrary
  return (Op l o r)

-- sample (arbitrary :: Gen Expr)
instance Arbitrary Expr where
    arbitrary = frequency [(3, genExpr), (1, genBinExpr)]

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

ex1 = Op (Const 1) MulOp (Op (Op (Expo 2) MulOp (Op (Const 1) AddOp (Expo 2))) AddOp (Const 2))
ex3 = Op (Op (Const 1) AddOp (Const 2)) MulOp (Const 3)

prop_exprToPoly :: Expr -> Bool
prop_exprToPoly e = eval 1 e == evalPoly 1 (exprToPoly e)

--------------------------------------------------------------------------------
-- * A7
polyToExpr :: Poly -> [Int] --Expr
polyToExpr = undefined


-- polyToExpr :: Poly -> [Int] --Expr
-- polyToExpr p = n : polyToExpr x
--   where
--     n = last (toList p) * 2
--     x = tail (toList p)

-- poly = in poly case of
--   n@last (toListPoly) = (Const n)
--
--
-- positionen avgör om det är expo eller const
--
-- värdet avgör hur många ggr
--
--
--
-- Expo (position) * värdet
--
-- o = length (toList poly)
--
-- o == 1 => Const (värdet av o)
-- otherwise
--
-- Expo ((length (toList poly)) .. Expo (length (toList poly) -1)

-- reverse [2,0,0,0,0] tail list = 0
-- length list = 5, 0, x^1, 1, x^2, 2 ... x^4 4

-- Write (and check) a quickCheck property for this function similar to
-- question 6.
prop_polyToExpr :: Poly -> Bool
prop_polyToExpr = undefined

--------------------------------------------------------------------------------
-- * A8
-- Write a function
simplify :: Expr -> Expr
-- which simplifies an expression by converting it to a polynomial
-- and back again
simplify = undefined

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property
prop_noJunk :: Expr -> Bool

--that checks that a simplified expression does not contain any "junk":
--where junk is defined to be multiplication by one or zero,
--addition of zero, addition or multiplication of numbers, or x to the
--power zero. (You may need to fix A7)

prop_noJunk = undefined

--------------------------------------------------------------------------------
