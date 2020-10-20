
data QA = Q QA QA String | Answer String
  deriving (Show, Read)

-- A answer answer question


-- t1 = Q "?" mm qe
--

defaultQA = Q y n "Is she from Europe? y/n"

-- Yes branch
y = Q mc qe "Is she a scientist?"
  mc = Answer "Marie Curie"
  qe = Answer "Queen Elizabeth II"

-- No branch
n = Q mm nno "Is she an actress?"
  mm = Answer "Marilyn Monroe"
  nno = Answer "Hilary Clinton"



-- question :: QA -> String
-- question (Q sq sq que) = do
--   putStrLn (que "")
--   a <- getLine
-- helper function for you
writeDefaultQA :: IO ()
writeDefaultQA = do
  writeFile "./Famous.qa" (show defaultQA)

tryIOError :: IO a -> IO (Either IOError a)
tryIOError = undefined

-- players will see this main function
main :: IO ()
main = do
  putStrLn "Welcome to the Game"
  inputFromFile <- readFile "./Famous.qa" -- TODO Famous.qa is not always there!
  -- inputFromFile :: String
  let qa = read inputFromFile :: QA
  -- GAME loop here
  gameLoop qa
  -- write the ONLINE qa to file
  return ()

gameLoop :: QA -> IO ()
gameLoop (Answer name) = do
  putStrLn name
gameLoop qa@(Q yes no question) = do
  putStrLn question
  userReply <- getLine
  case userReply of
    "y" -> gameLoop yes
    "n"  -> gameLoop no --(amendQA qa) --no

-- gameLoop yes =
--
-- yes == mm == Answer "Mariyn"

amendQA :: QA -> QA
amendQA _ = undefined


{-
question :: QA -> IO ()
question (Q sq sq que) = do
  putStrLn (que)
  a <-
-}
