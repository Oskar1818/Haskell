--Lab Tetris Part A
--Group 59
--Authors: Oskar Sturebrand, Valter Miari, Clara Josefsson

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
-- | can be empty or filled with a block of a specific colour.

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
-- | see <https://en.wikipedia.org/wiki/Tetromino>
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



-- * Some advanced functions ;)

-- ** A1
-- | emptyShape uses the standard function replicate who generates r elements of
-- | emptyRow s.
emptyShape :: (Int, Int) -> Shape
emptyShape (r, s) =  S (replicate r (emptyRow s))

emptyRow :: Int -> Row
emptyRow r = replicate r Nothing


-- ** A2
-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (S (x:xs)) = (length(x), length(x:xs))


-- ** A3
-- | Determines the length of a list comprehension, where in the list comp
-- | it iterates over the number of elements that isn't Nothing.
blockCount :: Shape -> Int
blockCount (S (x:xs)) = length [x | x <- join(S (x:xs)), x /= Nothing]

join :: Shape -> [Square]
join (S (x:xs)) = concat (x:xs)


-- * The Shape invariant

--  A4
-- | Shape invariant (shapes have at least one row, at least one column,
-- | and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (S (x:xs)) = shapeSize (S (x:xs)) >= (1, 1)
otherwise = False


-- * Test data generators

-- ** A5
-- | A random generator for colours
genColour :: Gen Colour -- sample genColour
genColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]

instance Arbitrary Colour where
  arbitrary = genColour


-- ** A6
-- | A random generator for shapes
genShape :: Gen Shape -- | sample genShape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape


-- * Transforming shapes

-- ** A7
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
-- | rotateShape = S . transpose . reverse
-- | rotateShape ransposes the rows and columns of reverse of the list xs.
rotateShape (S xs) = (S (transpose(reverse xs)))
-- | rotateShape S xs = S . transpose . reverse xs

-- ** A8
-- | shiftShape adds empty squares above and to the left of the shape
-- | i steps down and j steps to the right
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (i, j)  shape = S (shiftDown (i, j) (S (shiftRight (i, j) shape)) ++ shiftRight (i, j) shape)

shiftRight :: (w, Int) -> Shape -> [Row]
shiftRight (_, j) (S xs) = [(emptyRow j ++ x) | x <- xs]

shiftDown :: (Int, w) -> Shape -> [Row]
shiftDown (i, _) (S xs) = emp
  where (S emp) = emptyShape (i, shapeWidth (S xs))

-- | returns width of a shape
shapeWidth :: Shape -> Int
shapeWidth = length . head . rows
-- | other ways to write the function, worth remebering
  -- | shapeWidth s = length (head (rows s))
  -- | shapeWidth (S (x:xs)) = length(x)


-- ** A9
-- | padShape adds empty sqaure below and to the right of the shape
-- | shiftShape function is used on a 180° rotated shape,
-- | which is then rotated 180° again to generate the desired shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (i, j) s = doubleRotate (shiftShape (i, j) (doubleRotate s))

doubleRotate :: Shape -> Shape
doubleRotate s = rotateShape (rotateShape s)


-- ** A10
-- | pad a shape to a given size by
-- | adding differance between desired size and shape size, using shiftShape.
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (i, j) shape = shiftShape (i - c, j - r) shape
  where (c, r) = shapeSize shape


-- * Comparing and combining shapes

-- ** B1
-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = error "A11 overlaps undefined"

-- ** B2
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith = error "A12 zipShapeWith undefined"

-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- | The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = error "A13 zipShapeWith undefined"
