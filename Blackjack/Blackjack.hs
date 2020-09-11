module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

-- Size hand2 evaluated by hand
{-  size hand2
    = size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
    = 1 + size ((Card Jack Spades : []))
    = 1 + 1 + size []
    = 1 + 1 + 0
    = 2   -}

hand2 :: Hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

-- Describes the steps size takes
sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size ((Card Jack Spades : []))
            , 1 + 1 + size []
            , 1 + 1 + 0 -- might not be nececcary
            , 2
            ]

-- A test card
aCard1 :: Card
aCard1 = (Card Ace Spades)

-- A test card
aCard2 :: Card
aCard2 = (Card (Numeric 9) Diamonds)

-- A test hand
aHand :: Hand
aHand = [aCard1, aCard2]

-- to run in windows do
-- chcp 65001
-- then putStr (displayCard "anycard")
displayCard :: Card -> String
displayCard (Card r s) = displayRank r ++ " of " ++ displaySuit s

-- shows the rank
displayRank :: Rank -> String
displayRank (Numeric n) = show n -- gives the rank of numeric cards
displayRank r = show r -- gives the rank of suited card

-- Displays the suits with unicode characters
displaySuit :: Suit -> String
displaySuit Hearts = "\9829 "
displaySuit Spades = "\9824 "
displaySuit Diamonds = "\9830 "
displaySuit Clubs = "\9827 "
-- chcp 65001

-- Takes a hand as input, declares a basecase for it to be able recursively go over
-- the hand, then uses the help-functions to dislplay the cards on seperate lines
-- with \n
display :: Hand -> String
display [] = ""
display (x:xs) = displayCard x ++ "\n" ++ display xs

-- Declares the values of the ranks, as for the Ace; it evaluates to 1 in the
-- numberOfAces function, so it is not nececcary here
valueRank :: Rank -> Int
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10

-- This function ses first if the list is empty and then returns a empty String
-- then determines if there is an ace or not.
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (Card Ace _:restOfHand)  = 1 + numberOfAces restOfHand
numberOfAces (_:restOfHand) = numberOfAces restOfHand

-- sum [ 1 | card <- hand, (rank card) == Ace]

-- value
valueCard :: Card -> Int
valueCard (Card r s) = valueRank r

valueTot :: Hand -> Int
valueTot [] = 0
valueTot (x:xs) = valueCard x + valueTot xs

-- This function calculates if the value overexceedes 21 and if so, if it contains an or several
-- aces it converts them to 1 instead of 11.
value :: Hand -> Int
value hand | valueTot hand > 21 = valueTot hand - (10 * numberOfAces hand)
           | otherwise = valueTot hand


gameOver :: Hand -> Bool
gameOver hand = value hand > 21

winner :: Hand -> Hand -> Player
winner handGuest handBank | gameOver (handBank) = Guest
                          | gameOver (handGuest) = Bank
                          | value(handBank) > value(handGuest) = Bank
                          | value(handGuest) > value(handBank) = Guest
                          | value(handGuest) == value(handBank) = Bank
