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

empty :: [a]
empty = []

rand :: [Double]
rand = [0.75282073,1.914072e-2,0.25675058,0.20798653,0.36509913,0.6199135,0.552688,0.83191794,0.15301853,0.11863166,0.88036937,0.2985288,0.9647622,0.94379723,0.6323673,0.42535686,0.5850328,0.80125904,0.69982773,0.51680106,0.6051177,0.36048424,0.6814409,0.13755643,0.5730732,0.9105347,0.9013001,0.96504617,0.13346481,0.74288315,0.94035643,0.16613853,0.3206522,0.97053397,0.7711101,0.9005442,0.54222107,0.2805823,4.6526194e-3,9.8266125e-2,0.8040339,0.59864,0.74254036,0.6475005,0.23663306,0.2063654,0.44996387,0.9819821,0.43996918,0.7625793,0.12609422,0.45685077]

rand2 :: [Double]
rand2 = [0.13590,0.32166,0.25522,0.03182,0.54340,0.48190,0.04678,0.33925,0.77429,0.14017,0.94137,0.94746,0.66910,0.03446,0.08330,0.30840,0.00081,0.34435,0.62176,0.06847,0.78836,0.52718,0.58094,0.63772,0.69752,0.53751,0.45044,0.30676,0.25129,0.90626,0.34379,0.26909,0.35635,0.81598,0.26939,0.99049,0.61290,0.08187,0.88005,0.16828,0.02008,0.67079,0.09386,0.80489,0.45465,0.68393,0.33851,0.95474,0.08446,0.04079,0.53937,0.18388]

rand3 :: [Double]
rand3 = [0.67685,0.41155,0.88418,0.59005,0.17488,0.80204,0.02232,0.80384,0.76230,0.58706,0.02454,0.36806,0.32262,0.45520,0.76453,0.17119,0.36925,0.91135,0.77923,0.68156,0.77174,0.11998,0.31738,0.11296,0.79442,0.68355,0.83770,0.07661,0.04296,0.94470,0.72399,0.43367,0.56015,0.76472,0.67525,0.62652,0.86616,0.17025,0.45669,0.82064,0.20390,0.15895,0.02291,0.70323,0.59863,0.32813,0.56962,0.97155,0.24388,0.28274,0.00392,0.31269]

rand4 :: [Double]
rand4 = [0.36676,0.36359,0.01925,0.81437,0.94466,0.51992,0.08889,0.88665,0.24676,0.16677,0.67025,0.27830,0.94485,0.84745,0.82142,0.30912,0.08861,0.34710,0.08957,0.42999,0.04009,0.98664,0.57641,0.88698,0.33154,0.35167,0.57770,0.64807,0.23389,0.90014,0.24253,0.67716,0.73080,0.41494,0.80453,0.07092,0.37603,0.67955,0.81989,0.77273,0.53856,0.65133,0.33348,0.51640,0.32609,0.34647,0.54605,0.73880,0.75907,0.37479,0.71500,0.52476]
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
playBank deck = playBank' deck emptyHand -- probably "bankHand" with wrapper


-- Task B4 - klar :)                -- to test in win, with signs do:
shuffle :: [Double] -> Deck -> Deck -- putStr(display(shuffle rand4 fullDeck))
shuffle rand deck = [getcard x deck| x <- (randomInteger rand)]

randomInteger :: [Double] -> [Int]
randomInteger (x:xs) = [floor(52*x) | x <- (x:xs)]

getcard :: Int -> Deck -> Card -- Returns the n:th card in a Deck
getcard i d = d!!(i)


-- Task B5
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = length randomlist + length deck == length (shuffle randomlist deck) + length deck

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
