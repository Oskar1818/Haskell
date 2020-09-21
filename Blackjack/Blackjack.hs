-- Lab 2, Blackjack
-- Authors: Clara Josefsson, Oskar Sturebrand, Valter Miari
-- Lab group: 59

module Blackjack where
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)


-- Cards and hands
hand2 :: Hand -- A hand for sizeSteps
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]


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

{- Takes a hand as input, declares a basecase for it to be able recursively go over
he hand, then uses the help-functions to dislplay the cards on seperate lines
with \n -}

display :: Hand -> String
display [] = ""
display (x:xs) = displayCard x ++ "\n" ++ display xs

{-Declares the values of the ranks, as for the Ace; it evaluates to 1 in the
numberOfAces function, so it is not nececcary here -}
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

-- value
valueCard :: Card -> Int
valueCard (Card r s) = valueRank r

valueTot :: Hand -> Int
valueTot [] = 0
valueTot (x:xs) = valueCard x + valueTot xs

{- This function calculates if the value overexceedes 21 and if so,
if it contains an or several aces it converts them to 1 instead of 11. -}
value :: Hand -> Int
value hand | valueTot hand > 21 = valueTot hand - (10 * numberOfAces hand)
          | otherwise = valueTot hand

-- If the hand value is over 21, it's game over
gameOver :: Hand -> Bool
gameOver hand = value hand > 21
{- Compares the different cases in wich the game can end and
then determines the winner -}
winner :: Hand -> Hand -> Player
winner handGuest handBank
                          | gameOver (handGuest) = Bank
                          | gameOver (handBank) = Guest
                          | value(handGuest) < value(handBank) = Bank
                          | value(handGuest) > value(handBank) = Guest
                          | value(handGuest) == value(handBank) = Bank




--Task B1
ranks :: [Rank] -- List of all ranks
ranks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

suits :: [Suit] -- List of all suits
suits = [Hearts, Diamonds, Spades, Clubs]

fullDeck :: Deck -- A standard deck with all 52 cards
fullDeck = [ Card rank suit | rank <- ranks, suit <- suits]

prop_size_fullDeck :: Bool -- returns true if fullDeck contains 52 cards
prop_size_fullDeck = size fullDeck == 52


-- Task B2
-- Draws card from a deck and puts it in a hand
draw :: Deck -> Hand -> (Deck, Hand)
draw deck hand
   | size deck < 1 = error "draw: the deck is empty"
   | otherwise = ((drop 1 deck), (head deck : hand))


-- Task B3
{- Takes a deck and a hand, if the value of the bankHand is less than 16,
it calls on the draws function, until the value is 16 or greater. -}
playBank' :: Deck -> Hand -> Hand
playBank' deck bankHand
  | value bankHand < 16 = playBank' deck' bankHand'
  | otherwise = bankHand
    where (deck', bankHand') = draw deck bankHand

playBank :: Deck -> Hand
playBank deck = playBank' deck []


-- Task B4
-- putStr(display(shuffle rand4 fullDeck))
-- Rand r <- generate arbitrary usefull way to generate [Double]
shuffle :: [Double] -> Deck -> Deck
shuffle rand deck = shuffle' rand deck []

{- A recursive function that fist declares the basecase; where the oldDeck is empty
then calls the function again, recursively, this time with the oldDeck where the x card is removed
and the newDeck has the x card -}
shuffle' :: [Double] -> Deck -> Deck -> Deck
shuffle' rand [] newDeck = newDeck
shuffle' (x:xs) oldDeck newDeck = shuffle' xs oldDeck' (x':newDeck)
  where
    x' = getCard (randomInteger x oldDeck) oldDeck -- a card
    oldDeck' = removeCard (randomInteger x oldDeck) oldDeck

{- Takes a Double and the current size of the deck, then retuns a Int as to not
process the same possition in the deck twice -}
randomInteger :: Double -> Deck -> Int
randomInteger d deck = ceiling (size deck * d) -- [ceiling(52*x) | x <- (x:xs)]

-- Takes a deck and an Integer n, and retuns a card in the n-1:th place in a list.
getCard :: Int -> Deck -> Card
getCard n deck = deck!!(n-1)

-- Takes a deck and an Integer and returns everything but the n-1:th card in a deck.
removeCard :: Int -> Deck -> Deck
removeCard n deck = take (n-1) deck ++ drop n deck


-- Task B5
-- Determines if a card belongs to a deck
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

-- Determines if the deck contains the same cards after it's shuffled
prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

-- Determines if  the lenght of a deck is the same after the deck is shuffled
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = length deck == length (shuffle randomlist deck)


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
