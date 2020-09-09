module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

{-  size hand2
    = size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
    = 1 + size ((Card Jack Spades : []))
    = 1 + 1 + size []
    = 1 + 1 + 0
    = 2   -}

hand2 :: Hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size ((Card Jack Spades : []))
            , 1 + 1 + size []
            , 1 + 1 + 0 -- might not be nececcary
            , 2
            ]
