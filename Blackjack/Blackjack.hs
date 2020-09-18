-- Lab 2, Blackjack
-- Authors: Clara Josefsson, Oskar Sturebrand, Valter Miari
-- Lab group: 59

module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

-- Cards and hands

hand2 :: Hand -- Another test hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

hand3 :: Hand
hand3 = [Card (Numeric 10) Hearts, Card (Numeric 10) Spades, Card (Numeric 10) Diamonds]

hand4 :: Hand
hand4 = [Card (Numeric 10) Hearts, Card (Numeric 10) Spades, Card (Numeric 4) Diamonds]

-- Task A1
sizeSteps :: [Int] -- Prints a list of size hand for every step in sizeSteps
sizeSteps =
   [ size hand2 -- Between the [], size hand2 is evaluated by hand.
     , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
     , 1 + size ((Card Jack Spades : []))
     , 1 + 1 + size []
     , 1 + 1 + 0
     , 2 ]


-- Task A2
-- to run in windows do
-- chcp 65001
-- then putStr (displayCard "anycard")
displayCard :: Card -> String -- Displays the card in a readable way.
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
winner handGuest handBank
                          | gameOver (handGuest) = Bank
                          | gameOver (handBank) = Guest
                          | value(handGuest) < value(handBank) = Bank
                          | value(handGuest) > value(handBank) = Guest
                          | value(handGuest) == value(handBank) = Bank




--Task B1
ranks :: [Rank]
ranks = [Ace, King, Queen, Jack] ++ [Numeric n | n <- [2..10]]

suits :: [Suit]
suits = [Hearts, Diamonds, Spades, Clubs]

fullDeck :: Deck
fullDeck = [ Card rank suit | rank <- ranks, suit <- suits]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52


-- Task B2
draw :: Deck -> Hand -> (Deck, Hand)
draw deck hand
   | size deck < 1 = error "draw: the deck is empty"
   | otherwise = ((drop 1 deck), (head deck : hand))
--draw deck hand = (Deck, Hand)


-- Task B3
playBank' :: Deck -> Hand -> Hand
playBank' deck bankHand
  | value bankHand < 16 = playBank' deck' bankHand'
  | otherwise = bankHand
    where (deck', bankHand') = draw deck bankHand

playBank :: Deck -> Hand
playBank deck = playBank' deck [] -- probably "bankHand" with wrapper


-- Task B4
-- putStr(display(shuffle rand4 fullDeck))
shuffle :: [Double] -> Deck -> Deck
shuffle rand deck = shuffle' rand deck []

shuffle' :: [Double] -> Deck -> Deck -> Deck
shuffle' rand [] newDeck = newDeck
shuffle' (x:xs) oldDeck newDeck = shuffle' xs oldDeck' (x':newDeck)
  where
    x' = getCard (randomInteger x oldDeck) oldDeck -- a card
    oldDeck' = removeCard (randomInteger x oldDeck) oldDeck

randomInteger :: Double -> Deck -> Int
randomInteger d deck = ceiling (size deck * d) --[ceiling(52*x) | x <- (x:xs)]

getCard :: Int -> Deck -> Card -- Returns the n:th card in a Deck
getCard i d = d!!(i-1)

removeCard :: Int -> Deck -> Deck
removeCard i deck = take (i-1) deck ++ drop i deck


-- Task B5
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = length deck == length (shuffle randomlist deck)

{-testF = do
  Rand r <- generate arbitrary
  undefined -}

-- Task B6 -- exeption, stack overflow
implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation
