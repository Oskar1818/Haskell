-- Lab 3, Tetris
-- Authors: Clara Josefsson, Oskar Sturebrand, Valter Miari
-- Lab group: 59

-- | The Tetris game (main module)
module Main where

import ConsoleGUI       -- cabal install ansi-terminal
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes

import Test.QuickCheck

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation

-- | The state of the game
data Tetris = Tetris (Vector, Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int, Int)

-- | The size of the well
wellSize :: (Int, Int)
wellSize   = (wellWidth, wellHeight)
wellWidth  = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1, y1) `vAdd` (x2, y2) = (x1 + x2, y1 + y2)

-- | Move the falling piece into position
place :: (Vector, Shape) -> Shape
place (v, s) = shiftShape v s

-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (_, s) _ _) = prop_Shape s && wellSize == (10,20)

-- | Add black walls around a shape
-- | addBlackRow checks the size of the mapped black-shape uses the replicate
-- function to add the right amount of Just Black:s
addWalls :: Shape -> Shape
addWalls (S xs) = S $ addBlackRow (S xs) ++ map (addBlack) xs ++ addBlackRow (S xs)
 where
  addBlack :: Row -> Row
  addBlack xs = Just Black : xs ++ [Just Black]

  addBlackRow :: Shape -> [Row]
  addBlackRow (S xs) = ([blackRow])
   where
     (columns, rows) = shapeSize (S (map (addBlack) xs))
     blackRow        = replicate columns (Just Black)


-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v, s) w _) = addWalls (combine (shiftShape v s) w)

-- Moves a shape by vector steps
move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v2, s) w d) = (Tetris ((v1 `vAdd` v2), s) w d)

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris ds = Tetris (startPosition, shape1) (emptyShape wellSize) supply
  where
    shape1:supply = genShapes ds
    -- picking a shape from the infinite list.

    ls = fromIntegral $ length allShapes
    -- converts the lenght from int to double.

    randInt :: Double -> Int
    randInt d = floor $ d * ls
    -- generates a relevant random double which is converted to int by floor.

    genShapes :: [Double] -> [Shape]
    genShapes (d:ds) = allShapes !! randInt d : genShapes ds
    -- a recursive function wich creates an infinite list, which is ok
    -- due to Haskell being lazy.


-- returns the new state of the game, where the new tetris-state is modified
-- by the move-function.
tick :: Tetris -> Maybe (Int, Tetris)
tick t@(Tetris (v, s) w sup) =
  if collision t'
    then dropNewPiece t -- call dropNewPiece
    else Just (0, t')     -- if that fails, game over.else Just (0, move (0, 1) t )
      where t' = move (0, 1) t

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick t = tick t
stepTetris MoveLeft t = Just (0, movePiece (-1) t)
stepTetris MoveRight t = Just (0, movePiece 1 t)
stepTetris MoveDown t = tick t
stepTetris Rotate t = Just (0, rotatePiece t)



-- Checks if there is any collisions in a tetris
collision :: Tetris -> Bool
collision (Tetris ((v1, v2), s) w _)
  | place ((v1, v2), s) `overlaps` w     = True -- new piece collide?
  | v1 + fst (shapeSize s) > wellWidth   = True -- too far right?
  | v2 + snd (shapeSize s) > wellHeight  = True -- too far down?
  | v1 < 0                               = True -- too far left?
  | otherwise                            = False -- no collision

-- Moves a piece in the x dimension
movePiece :: Int -> Tetris -> Tetris
movePiece i t@(Tetris ((x, y), s) w sup)
  | collision $ move (i, 0) t = t
  | otherwise                 = move (i, 0) t

-- rotates a shape in a tetris
rotate:: Tetris -> Tetris
rotate (Tetris ((v1, v2), s) w sup) = (Tetris ((v1, v2), (rotateShape s)) w sup)

rotatePiece :: Tetris -> Tetris
rotatePiece t@(Tetris ((v1, v2), s) w sup)
  | collision $ rotate t = t
  | otherwise            = rotate t


-- Game ends if the new piece collides, else it returns a new random piece
-- which is combined with the well, which has passed through clearLines.
dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece t@(Tetris ((x, y), s) w sup)
  | collision tNew = Nothing -- game over
  | otherwise = Just (n, tNew)
    where
     tNew = (Tetris (startPosition, s') w' sup')
     s' = head sup
     (n, w') = clearLines $ combine (place((x,y), s)) w
     sup' = tail sup


clearLines :: Shape -> (Int, Shape)
clearLines (S xs) = (cleared, shape')
  where
    -- adds empty rows ontop of the remaining well (shape)
    shape'    = shiftShape (0,cleared) (S remaining)  -- the new shape with the rows cleared and filled with empty rows
    -- the that should be cleared, and added as empty rows
    cleared   = length xs - length remaining -- the amount of rows left
    -- filters away the rows that are full and returns the remaining well
    remaining = filter notFull xs -- all rows that istn't null

-- evaluates to false if a row is full; to make efficient use of filter func.
notFull :: Row -> Bool
notFull xs = not $ and $ map (\l -> l /= Nothing) xs
