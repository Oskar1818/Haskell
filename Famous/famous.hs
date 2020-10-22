import System.IO.Error
import Text.Read (readMaybe)
import Data.Char

data QA = Q QA QA String | Answer String -- TODO option for follow up question after answer
  deriving (Show, Read)

-- data Either a b = Left a | Right b


defaultQA = Q y n "Is she from Europe? y/n"

-- Yes branch
y = Q mc qe "Is she a scientist?"
mc = Answer "Marie Curie"
qe = Answer "Queen Elizabeth II"

-- No branch
n = Q mm nno "Is she an actress?"
mm = Answer "Marilyn Monroe"
nno = Answer "Hilary Clinton"

writeDefaultQA :: IO ()
writeDefaultQA = do
  writeFile "./Famous.qa" (show defaultQA)


-- players will see this main function
main :: IO ()
main = do
  putStrLn "Welcome to the Game, please answer the questions with y/n."
  inputFromFile <- tryIOError (readFile "./Famous.qa")
  case inputFromFile of
    Left _ -> do
      putStrLn "Huston we have a problem"
      writeDefaultQA -- good idea or bad idea?
      play defaultQA
    Right s -> case readMaybe s of
      Nothing -> play defaultQA
      Just qa -> play qa

  -- inputFromFile :: String
  -- let qa = read inputFromFile :: QA
  -- GAME loop here
  -- play qa
  -- write the ONLINE qa to file, when the gameLoop ends. TODO Write the tree to Famous.qa
  -- return ()

play :: QA -> IO ()
play qa = do
  qa' <- gameLoop qa
  playAgain <- yesNoQuestion "Would you like to play again??"
  case playAgain of
    True -> play qa'
    False -> do
      writeFile "./Famous.qa" (show qa')
      return ()


gameLoop :: QA -> IO QA
gameLoop (Answer name) = do
  conf <- yesNoQuestion $ "Did you think of " ++ name ++ "?"
  case conf of
    True -> do
      putStr "I Win :D "
      return (Answer name)
--      play <- yesNoQuestion $ "I win! Would you like to play again?"
--      case play of
--        True -> return (Answer name) -- play again -- would like to have fresh qa
--        False -> return (Answer name)
    False -> do
      putStrLn "OK - you win this time!"
      inPerson <- question $ "Just curious, who was it? "
      inQuestion <- question $ "Give me a question, for which the answer for "
                    ++ inPerson
                    ++ " is \"yes\" \n and the answer for "
                    ++ name
                    ++ " is \"no\""
      return (Q (Answer inPerson) (Answer name) inQuestion)
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
question :: String -> IO String
question s = do
  putStrLn s
  a <- getLine
  return (a)

yesNoQuestion :: String -> IO Bool
yesNoQuestion s = do
  reply <- question s
  ynp reply
  {-
  case ynp reply of
    "y" -> return True
    "n" -> return False
-}
ynp :: String -> IO Bool
ynp ('Y':_) = return True
ynp ('y':_) = return True
ynp _ = return False

-- Read RWH page 344 Chp 14 Monad
-- "do" pretty way of writing >>=
-- do {}
-- bind (>>=)
