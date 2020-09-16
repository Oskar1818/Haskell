import System.Random
baz :: [Float] -> Int -> IO [Float]
baz xs 52 = return xs
baz xs c = do
    seed  <- newStdGen
    let rs = fst $ randomR (0,1) seed
    baz (rs:xs) (c + 1)
-- baz [] 0
