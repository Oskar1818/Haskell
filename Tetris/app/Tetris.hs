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

move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v2, s) w d) = (Tetris ((v1 `vAdd` v2), s) w d)

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris ds = Tetris (startPosition, shape1) (emptyShape wellSize) supply
  where
    shape1:supply = genShapes ds
    ls = fromIntegral $ length allShapes

    randInt :: Double -> Int
    randInt d = floor $ d * ls

    genShapes :: [Double] -> [Shape]
    genShapes (d:ds) = allShapes !! randInt d : genShapes ds


-- returns the new state of the game, where the new tetris-state is modified
-- by the move-function.
tick :: Tetris -> Maybe (Int, Tetris)
tick t@(Tetris (v, s) w sup) =
                             if (collision t == True)
                              then dropNewPiece t -- call dropNewPiece
                                               -- if that fails, game over.
                              else Just (0, move (0, 1) t )

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick t = tick t
stepTetris MoveLeft t = tick $ movePiece (-1) t
stepTetris MoveRight t = tick $ movePiece 1 t
stepTetris MoveDown t = tick t
stepTetris Rotate t = tick $ rotatePiece t
stepTetris _ t = Just (0,t) -- incomplete !!!


collision :: Tetris -> Bool
collision (Tetris ((v1, v2), s) w _)
  | place ((v1, v2), s) `overlaps` w     = True
  | v1 + fst (shapeSize s) > wellWidth   = True
  | v2 + snd (shapeSize s) >= wellHeight = True
  | v1 < 0                               = True
  | otherwise                            = False

movePiece :: Int -> Tetris -> Tetris
movePiece i t@(Tetris ((x, y), s) w sup)
  | collision $ move (i, 0) t = t
  | otherwise                 = move (i, 0) t

rotate:: Tetris -> Tetris
rotate (Tetris ((v1, v2), s) w sup) = (Tetris ((v1, v2), (rotateShape s)) w sup)

rotatePiece :: Tetris -> Tetris
rotatePiece t@(Tetris ((v1, v2), s) w sup)
  | collision $ rotate t = t
  | otherwise            = rotate t


dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris ((x, y), s) w sup)
  | s' `overlaps` w = Nothing
  | otherwise = Just (0, (Tetris ((x, y), s') w' sup'))
    where
     s' = place ((startPosition), head sup)
     w' = (combine (shiftShape (startPosition) (head sup)) w )
     sup' = tail sup

--Tetris (startPosition, head sup) (w, fast modified) (tail sup)



{-test, not complete
addPieceToWell :: Shape -> Tetris
addPieceToWell s = (Tetris ((x, y), s) (combine (shiftShape (startPosition) s) w) sup) -}

-- if tetris, has collided, then return (Int, Tetris)
-- dropNewPiece :: Tetris -> Maybe (Int, Tetris)
-- dropNewPiece t = collision t
--    then tick $ startTetris something
--

{- addPieceToWell :: Shape -> Tetris
addPieceToWell s = (Tetris ((x, y), s) (combine (shiftShape v s) w) sup) -}
