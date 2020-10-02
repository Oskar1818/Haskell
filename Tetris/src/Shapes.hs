-- Lab 3, Tetris
-- Authors: Clara Josefsson, Oskar Sturebrand, Valter Miari
-- Lab group: 59


-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Test.QuickCheck

-- * Shapes
type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
data Shape = S [Row] deriving (Eq)
type Row   = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes
showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]

    showSquare Nothing      = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game
-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes]
   where
      makeSquares = map (map colour)
      colour c    = lookup c [ ('I', Red), ('J', Grey), ('T', Blue)
                             , ('O', Yellow), ('Z',Cyan), ('L', Green)
                             , ('S', Purple) ]
      shapes =
              [["I",
                "I",
                "I",
                "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]


-- * Some simple functions

-- ** A1
emptyShape :: (Int, Int) -> Shape
emptyShape (x', y) =  S (replicate y (emptyRow x'))

emptyRow :: Int -> Row
emptyRow x' = replicate x' Nothing


-- ** A2
-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (S (x:xs)) = (length(x), length(x:xs))

-- ** A3
-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S (x:xs)) = length [x | x <- join(S (x:xs)), x /= Nothing]

-- | Helperfunction to aid blockCount, returns individual list of squares from shape
join :: Shape -> [Square]
join (S (x:xs)) = concat (x:xs)



-- * The Shape invariant

-- ** A4
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
-- | divides the sum of all row lenghts with lenght of a single row, and
-- then check if the average row is the same as the single row.
prop_Shape :: Shape -> Bool
prop_Shape (S xs) = rowL == avgL && columns
 where
   columns = shapeSize (S xs) >= (1, 1)
   rowL = map length xs
   avgL = div (sum rowL) (length rowL)

-- * Test data generators

-- ** A5
-- | A random generator for colours
-- | Uses the QuickCheck-function elements wich works as follows: [a] -> Gen a
genColour :: Gen Colour
genColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]

instance Arbitrary Colour where
  arbitrary = genColour


-- ** A6
-- | A random generator for shapes
genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape


-- * Transforming shapes

-- ** A7
-- | Rotates a shape 90° clockwise
rotateShape :: Shape -> Shape
rotateShape (S xs) = (S (transpose(reverse xs)))


-- ** A8
-- | shiftShape adds empty squares above and to the left of the shape
-- x' = number of steps right
-- y = number of steps down
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (x', y)  s1 = S (shiftDown (x', y) (S (shiftRight (x', y) s1)) ++ shiftRight (x', y) s1)

-- Adds empty squares to the left of a shape
shiftRight :: Int -> Shape -> [Row]
shiftRight x' (S xs) = [(emptyRow x' ++ x) | x <- xs]

-- Adds empty squares above a shape
shiftDown :: Int -> Shape -> [Row]
shiftDown y (S xs) = emp
  where (S emp) = emptyShape (shapeWidth (S xs), y)

-- Returns the width of a given shape
shapeWidth :: Shape -> Int
shapeWidth (S (x:xs)) = length(x)


-- ** A9
-- | padShape adds empty sqaure below and to the right of the shape
-- | x' steps to the left
-- | y steps uppwords
padShape :: (Int, Int) -> Shape -> Shape
padShape (x', y) s1 = doubleRotate (shiftShape (x', y) (doubleRotate s1))

-- rotates a shape 180° clockwise
doubleRotate :: Shape -> Shape
doubleRotate s1 = rotateShape (rotateShape s1)


-- ** A10
-- | pads a shape to a given size by adding empty rows and columns above and to the right.
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (x', y) s1 = padShape (x' - c, y - r) s1
  where (c, r) = shapeSize s1


-- * Comparing and combining shapes

-- ** B1
-- | Tests if two shapes overlaps
overlaps :: Shape -> Shape -> Bool
overlaps (S sh1) (S sh2) = or $ zipWith rowsOverlap sh1 sh2

-- tests if two rows overlap
rowsOverlap :: Row -> Row -> Bool
rowsOverlap xs ys = or [(x /= Nothing) && (y /= Nothing) | x <- xs, y <- ys]


zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f (S s1) (S s2) = S $ zipWith (zipWith f) s1 s2

-- example function
blackClashes :: Shape -> Shape -> Shape
blackClashes s1 s2 = zipShapeWith clash s1 s2
 where
  clash :: Square -> Square -> Square
  clash Nothing Nothing = Nothing
  clash Nothing s       = s
  clash s       Nothing = s
  clash (Just c1) (Just c2) = Just Black


-- ** B3
-- Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
combine s1 s2
  | s1 `overlaps` s2 = error "The shapes overlap"
  | otherwise = (zipShapeWith f' (padShapeTo x s1) (padShapeTo x s2))

 where
   x = convert s1 s2

f' :: Square -> Square -> Square
f' Nothing Nothing = Nothing
f' Nothing s       = s
f' s       Nothing = s

-- Returns the largest cordinate value, of each shape
convert :: Shape -> Shape -> (Int, Int)
convert (S s1) (S s2) = (max x a, max y b)
 where
  (x, y) = shapeSize (S s1)
  (a, b) = shapeSize (S s2)

-- | Types and functions for shapes. The list of all tetris pieces.
