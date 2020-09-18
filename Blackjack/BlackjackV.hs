-- Lab 2, Blackjack
-- Authors: Clara Josefsson, Oskar Sturebrand, Valter Miari
-- Lab group: 59

module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

-- Cards and hands
aCard1 :: Card -- A test card
aCard1 = (Card Ace Spades)

aCard2 :: Card -- Another test card
aCard2 = (Card (Numeric 9) Diamonds)

aCard3 :: Card
aCard3 = (Card (Numeric 7) Clubs)

aHand :: Hand -- A test hand
aHand = [aCard1, aCard2]

emptyHand :: Hand
emptyHand = []

hand2 :: Hand -- Another test hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

aDeck2 :: Deck
aDeck2 = [aCard1, aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1,aCard1]

aDeck :: Deck
aDeck = [aCard1, aCard2, aCard1]

aBustHand :: Hand
aBustHand = [aCard2, aCard2, aCard2]

empty :: [a]
empty = []

rand = [0.75282073,1.914072e-2,0.25675058,0.20798653,0.36509913,0.6199135,0.552688,0.83191794,0.15301853,0.11863166,0.88036937,0.2985288,0.9647622,0.94379723,0.6323673,0.42535686,0.5850328,0.80125904,0.69982773,0.51680106,0.6051177,0.36048424,0.6814409,0.13755643,0.5730732,0.9105347,0.9013001,0.96504617,0.13346481,0.74288315,0.94035643,0.16613853,0.3206522,0.97053397,0.7711101,0.9005442,0.54222107,0.2805823,4.6526194e-3,9.8266125e-2,0.8040339,0.59864,0.74254036,0.6475005,0.23663306,0.2063654,0.44996387,0.9819821,0.43996918,0.7625793,0.12609422,0.45685077]

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
winner handGuest handBank | gameOver (handBank) = Guest
                         | gameOver (handGuest) = Bank
                         | value(handBank) > value(handGuest) = Bank
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

draw :: Deck -> Hand -> (Deck, Hand)
draw deck hand
   | size deck < 1 = error "draw: the deck is empty"
   | otherwise = ((drop 1 deck), (head deck : hand))
--draw deck hand = (Deck, Hand)

playBank' :: Deck -> Hand -> Hand
playBank' deck bankHand
  | value bankHand < 16 = playBank' deck' bankHand'
  | otherwise = bankHand
    where (deck', bankHand') = draw deck bankHand

playBank :: Deck -> Hand
playBank deck = playBank' deck emptyHand -- probably "bankHand" with wrapper

shuffle :: [Double] -> Deck -> Deck
shuffle = undefined

randomInteger :: [Double] -> [Int]
randomInteger (x:xs) = [floor(52*x) | x <- (x:xs)]

getnthCard :: [Int] -> Deck -> (Card, Deck)
getnthCard lint deck = -- match

getnthcard :: [Int] -> Deck -> (Card, Deck)
getnthcard lint deck = [(i,c) | i <- lint,
                                c <- deck ]
-- [(i,j) | i <- [1,2],
--         j <- [1..4] ]
randomInteger :: [Double] -> [Int]
--randomInteger (x:xs) = floor(52*x)
randomInteger (x:xs) = [floor(52*x) | x <- (x:xs)]
-- randomInteger (x:xs) = x randomInteger x-xs -- 52 * 0.2 = 10.4 ~ 10
-- valueTot (x:xs) = valueCard x + valueTot xs
-- randomInteger (x:xs) = randomInteger floor(x*52) -- 52 * 0.2 = 10.4 ~ 10
-- randomInteger (x:xs) =


{-
[0.2,0.3,0.4,....]
(x:xs)
x = 0.2
xs = .....


x = 0.2
deck = 52 cards
generate a numnber between 0 and 51 using 0.2
5 * x * 10

52 * 0.2 = 10.4 ~ 10
