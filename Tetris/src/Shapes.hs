
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
emptyShape (r, s) =  S (replicate r (emptyRow s))

emptyRow :: Int -> Row
emptyRow r = replicate r Nothing


-- ** A2

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (S (x:xs)) = (length(x), length(x:xs))

-- ** A3

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S (x:xs)) = length [x | x <- join(S (x:xs)), x /= Nothing]

aShape :: Shape
aShape = emptyShape(3, 2)

aShape2 :: Shape
aShape2 = emptyShape(1, 0)

join :: Shape -> [Square]
join (S (x:xs)) = concat (x:xs)

-- * The Shape invariant

-- ** A4
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (S (x:xs)) = shapeSize (S (x:xs)) >= (1, 1)


-- * Test data generators

-- ** A5
-- | A random generator for colours
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
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (S (xs)) = (S (transpose(reverse xs)))


-- ** A8
-- | shiftShape adds empty squares above and to the left of the shape
-- i är antal steg nedåt.
-- j är antal steg åt höger
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (i, j)  (S (xs)) = S (shiftDown (i, j) (S (shiftRight (i, j) (S (xs)))) ++ shiftRight (i, j) (S (xs)))

shiftRight :: (w, Int) -> Shape -> [Row]
shiftRight (_, j) (S (xs)) = [(emptyRow j ++ x) | x <- xs]

shiftDown :: (Int, w) -> Shape -> [Row]
shiftDown (i,_) (S (xs)) = emp
  where (S emp) = emptyShape (i, (shapeWidth (S (xs))))

shapeWidth :: Shape -> Int
shapeWidth (S (x:xs)) = length(x)

-- ** A9
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (i, j) s = doubleRotate (shiftShape (i, j) (doubleRotate s))

doubleRotate :: Shape -> Shape
doubleRotate s = rotateShape (rotateShape s)

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (i, j) shape = padShape (i - r, j - c) shape
  where (c, r) = shapeSize shape

-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
overlaps (S sh1) (S sh2) = or $ zipWith rowsOverlap sh1 sh2

rowsOverlap :: Row -> Row -> Bool
rowsOverlap xs ys = or [(x /= Nothing) && (y /= Nothing) | x <- xs, y <- ys]

-- ** B2
-- ** B2
-- | zipShapeWith, like 'zipWith' for lists

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- :: ((a -> b -> c) -> [a] -> [b] -> [c]) -> ((a -> b -> c) -> [a] -> [b] -> [c]) -> ?


{- f :: a -> b


f == (\x -> f x) -}

zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f (S s1) (S s2) = S $ zipWith (zipWith f) s1 s2 --zipShape . zipShape'

  {- where
    zipRows :: (Square -> Square -> Square) -> Row -> Row -> Row
    zipRows f row1 row2 = zipWith f row1 row2 -}

{-
zipShape :: (a -> b -> c) -> [a] -> [b] -> [c]
zipShape f xs ys = S . map ((x,y) ->(zipShape' f) x y) . zip xs $ ys

zipShape' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipShape' f xs ys = map ((x,y) -> f x y) . zip xs $ ys
-}
{-
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith = zipShape . zipShape

zipShape :: (a -> b -> c) -> [a] -> [b] -> [c]
zipShape f xs ys = map ((x,y) -> f x y) . zip xs $ ys
-}
--map (S . xs) [list]


blackClashes :: Shape -> Shape -> Shape
blackClashes s1 s2 = zipShapeWith clash s1 s2
 where
  clash :: Square -> Square -> Square
  clash Nothing Nothing = Nothing
  clash Nothing s       = s
  clash s       Nothing = s
  clash (Just c1) (Just c2) = Just Black


-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
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


convert :: Shape -> Shape -> (Int, Int)
convert (S s1) (S s2) = (max y b, max x a)
 where
  (x, y) = shapeSize (S s1)
  (a, b) = shapeSize (S s2)

-- | Types and functions for shapes. The list of all tetris pieces.
