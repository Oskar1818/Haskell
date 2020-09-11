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


aCard1 :: Card
aCard1 = (Card Ace Spades)

aCard2 :: Card
aCard2 = (Card (Numeric 9) Diamonds)

aHand :: Hand
aHand = [aCard1, aCard2]

--hand :: Hand
--hand = [(Card r s), (Card f k)]

displayCard :: Card -> String
displayCard (Card r s) = displayRank r ++ " of " ++ show s


displayRank :: Rank -> String
displayRank (Numeric n) = show n
displayRank r = show r


display :: Hand -> String
display [(Card r s), (Card f k)] = (displayCard (Card r s)) ++ (displayCard (Card f k))


valueRank :: Rank -> Int
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10

numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (aCard1:restOfHand)  = 1 + numberOfAces restOfHand
numberOfAces (_:restOfHand) = numberOfAces restOfHand

valueCard :: Card -> Int
valueCard (Card r s) = valueRank r


value :: Hand -> Int
value hand | value hand > 21 = value hand - (10 * numberOfAces hand)
           | otherwise = value hand
value [] = 0
value [(Card r s), (Card f k)] = (valueCard (Card r s)) + (value [(Card r s), (Card f k)])


gameOver :: Hand -> Bool
gameOver hand = value hand > 21

winner :: Hand -> Hand -> Player
winner handGuest handBank | gameOver (handBank) = Guest
                          | gameOver (handGuest) = Bank
                          | value(handBank) > value(handGuest) = Bank
                          | value(handGuest) > value(handBank) = Guest
                          | value(handGuest) == value(handBank) = Bank
