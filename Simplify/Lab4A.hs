-- Authors:
-- Date:

import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp
  -- deriving Show

--------------------------------------------------------------------------------
-- * A1
-- Define a data type Expr which represents three kinds of expression:
-- binary operators (use BinOp as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should not use String or Char anywhere, since this is
-- not needed.

data Expr = Const Int
          | Expo Int
          | Op Expr BinOp Expr
            -- deriving Show


--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
-- prop_Expr :: Expr -> Bool
-- prop_Expr e@(Const x) = e == x
-- prop_Expr (Expo x) = x < 0
-- prop_Expr e@(Op e1 AddOp e2) = e == e1 + e2
-- prop_Expr e@(Op e1 MulOp e2) = e == e1 * e2

-- instance Arbitrary Expr where
--     arbitrary = prop_Expr


--------------------------------------------------------------------------------
-- * A3
-- Make Expr an instance of Show (along the lines of the example in the lecture)
-- You can use Haskell notation for powers: x^2
-- You should show x^1 as just x.
showExpr :: Expr -> String
showExpr e = case e of
  Const n        -> show n
  Expo n         -> " x^" ++ show n
  Op e1 AddOp e2 -> showExpr e1 ++ " + " ++ showExpr e2
  Op e1 MulOp e2 -> showMul  e1 ++ " * " ++ showMul  e2
  where
    showMul e @(Op e1 AddOp e2) = "(" ++ showExpr e ++ ")"
    showMul e                  = showExpr e

instance Show Expr where
   show = showExpr

--------------------------------------------------------------------------------
-- * A4
-- Make Expr and instance of Arbitrary.
-- Now you can check the data type invariant that you defined in A3 using
-- quickCheck

-- (Optional)
-- Add a definition of function shrink :: Expr -> [Expr] to Arbitrary
-- which gives hints to quickCheck on possible smaller expressions that it
-- could use to find a smaller counterexample for failing tests

 --prop_Expr

--------------------------------------------------------------------------------
-- * A5
-- Define the eval function which takes a value for x and an expression and
-- evaluates it

eval :: Int -> Expr -> Int
eval _ (Const n) = n
eval x (Expo n)  = x ^ n
eval x (Op e1 AddOp e2) = (eval x e1) + (eval x e2)
eval x (Op e1 MulOp e2) = (eval x e1) * (eval x e2)

--
-- evalPoly :: Int -> Poly -> Int
-- evalPoly x (Poly cs) = sum $ zipWith (*) cs powersOfx
--          where powersOfx = map (x^) [0..]

--------------------------------------------------------------------------------
-- * A6
-- Define
exprToPoly :: Expr -> Poly
-- Which converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way.

exprToPoly = undefined

-- Define (and check) prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression

prop_exprToPoly = undefined

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction,
polyToExpr :: Poly -> Expr

polyToExpr = undefined


-- Write (and check) a quickCheck property for this function similar to
-- question 6.
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
