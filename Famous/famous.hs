
data QA = Q QA QA String | Answer String
  deriving (Show, Read)



-- t1 = Q "?" mm qe
--
-- mm = Answer "Marilyn Monroe"
-- qe = Answer "Queen Elizabeth II"
--
-- parent tree = Q "Is she from Europe?"
--
-- question :: QA -> String
-- question (Q sq sq que) = do
--   putStrLn (que "")
--   a <- getLine

addingMachine :: Int -> IO ()
addingMachine acc = do
  putStrLn ("Sum so far: " ++ show acc)
  putStrLn ("Enter a number: ")
  n <- getLine
  addingMachine (acc + read n)
