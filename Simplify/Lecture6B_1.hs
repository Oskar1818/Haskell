{- |
Module      : Lecture6B_1
Description : Lecture week 6, part B, Arithmetic Quiz
Copyright   : (c) 2020 TDA555/DIT440, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

How to model and work with simple expression languages.
-}

module Lecture6B_1 where

import Control.Monad (forever)
import Test.QuickCheck

--------------------------------------------------------------------------------
-- * The main program

-- main :: IO ()
-- main = do
--   putStrLn "Welcome to the arithmetic quiz!"
--   putStr   "Please choose a difficulty (a positive number):\n> "
--   difficulty <- readLn
--   forever (quiz difficulty)
--
-- quiz :: Int -> IO ()
-- quiz difficulty = do
--   e <- generate $ genExpr difficulty
--   putStrLn ("What is " ++ showExpr e ++ "?")
--   answer <- readLn
--   let correct = eval e
--   putStrLn $ if answer == correct
--     then "Yes, that is correct!"
--     else "Sorry, the correct answer is: " ++ show correct

--------------------------------------------------------------------------------
-- | A representation of simple arithmetic expressions
data Expr = Con Int
          | Add Expr Expr
          | Mul Expr Expr
          deriving Show

ex1 = Con 2                              -- 2
ex2 = Add (Con 1) (Con 2)                -- 1 + 2
ex3 = Mul (Add (Con 1) (Con 2)) (Con 3)  -- (1+2)*3
ex4 = Add (Con 1) (Mul (Con 2) (Con 3))  -- 1+2*3

--------------------------------------------------------------------------------
-- | Evaluate (compute the value of) an expression
eval :: Expr -> Int
eval e = case e of
  Con n   -> n
  Add x y -> eval x + eval y
  Mul x y -> eval x * eval y

--------------------------------------------------------------------------------
-- | Showing expressions
showExpr :: Expr -> String
showExpr e = case e of
  Con n   -> show n
  Add x y -> showExpr x ++ " + " ++ showExpr y
  Mul x y -> showFactor x ++ " * " ++ showFactor y
 where
  showFactor :: Expr -> String
  showFactor e@(Add x y) = "(" ++ showExpr e ++ ")"
  showFactor e           = showExpr e

-- instance Show Expr where
--   show = showExpr

--------------------------------------------------------------------------------
-- * Generating arbitrary expressions

-- | Random generator (first version, with bad control of size)

genExpr :: Int -> Gen Expr
genExpr n = frequency [(1, genSimple), (n, genOp)]
 where
  genSimple = do
    op <- elements [Add, Mul]
    n  <- choose (0, 10)
    m  <- choose (0, 10)
    return (Con m `op` Con n)

  genOp = let m = n `div` 4 in do
    op <- elements [Add, Mul]
    x  <- genExpr m
    y  <- genExpr m
    return (x `op` y)

instance Arbitrary Expr where
  arbitrary = sized genExpr
