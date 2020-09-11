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

displayCard :: Card -> String
displayCard (Card r s) = displayRank r ++ " of " ++ show s
--displayCard [(Card r s)] =

displayRank :: Rank -> String
displayRank (Numeric n) = show n
displayRank r = show r


display :: Hand -> String
display [(Card r s), (Card f k)] = show ([Card r s, Card f k])


valueRank :: Rank -> Int
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10

numberOfAces :: Hand -> Integer
numberOfAces [] = 0
-- numberOfAces hand = numberOfAces hand
numberOfAces (aCard1:restOfHand)  = 1 + numberOfAces restOfHand

{-valueCard :: Card -> Int
valueCard (Card r s) = valueRank r



value :: Hand -> Int
value [] = 0
value (hand) = (valueCard) + (valueHand)

gameOver :: Hand -> Bool
gameOver = value > 21

winner :: Hand -> Hand -> Player
winner handGuest handBank | gameOver (handBank) = Guest
                          | gameOver (handGuest) = Bank
                          | value (handBank) > (handGuest) = Bank
                          | value (handGuest) > (handBank) = Guest
                          | value (handGuest) == (handBank) = Bank -}
