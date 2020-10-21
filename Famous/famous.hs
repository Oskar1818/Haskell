
data QA = Q QA QA String | Answer String -- TODO option for follow up question after answer
  deriving (Show, Read)

data Either a b = Left a | Right b


defaultQA = Q y n "Is she from Europe? y/n"

-- Yes branch
y = Q mc qe "Is she a scientist?"
mc = Answer "Marie Curie"
qe = Answer "Queen Elizabeth II"

-- No branch
n = Q mm nno "Is she an actress?"
mm = Answer "Marilyn Monroe"
nno = Answer "Hilary Clinton"

-- helper function for you
writeDefaultQA :: IO ()
writeDefaultQA = do
  writeFile "./Famous.qa" (show defaultQA)

-- TODO tryIOError :: IO a -> IO (Either IOError a)
-- tryIOError = undefined

-- players will see this main function
main :: IO ()
main = do
  putStrLn "Welcome to the Game"
  inputFromFile <- readFile "./Famous.qa" -- TODO Famous.qa is not always there!
  -- inputFromFile :: String
  let qa = read inputFromFile :: QA
  -- GAME loop here
  play qa
  -- write the ONLINE qa to file, when the gameLoop ends. TODO Write the tree to Famous.qa
  -- use amendQA
  return ()

play :: QA -> IO QA
play qa = do
  qa' <- gameLoop qa
  play qa'

gameLoop :: QA -> IO QA
gameLoop (Answer name) = do
  putStrLn $ "Best guess, " ++ name ++ ". Was that correct? y/n"
--
  return (Answer name)
  -- a <- getLine
  -- yesNoQuestion a
gameLoop qa@(Q yes no q) = do
  anw <- yesNoQuestion q
  case anw of
    True -> do
      yes' <- gameLoop yes
      return (Q yes' no q)
    False -> do
      no' <- gameLoop no
      return (Q yes no' q)

   -- if anw then gameLoop yes' else gameLoop no'


-- case yesNoQuestion q of
-- True  -> do yes' <- gameLoop yes
--              return (Q yes' no q)
-- False -> do no' <- gameLoop no
--              return (Q yes no' q) --(amendQA qa) --n

-- amendQA :: QA -> QA
-- amendQA _ = case yesNoQuestion q of
--     True  -> do yes' <- gameLoop yes
--                return (Q yes' no q)
--     False -> do no' <- gameLoop no
--                return (Q yes no' q)
--  writeFile "./Famous.qa" (the updated Tree aka QA)



question :: String -> IO String
question s = do
  putStrLn s
  a <- getLine
  return (a)

yesNoQuestion :: String -> IO Bool
yesNoQuestion s = do
  reply <- question s
  case reply of
    "y" -> return True
    "n" -> return False
