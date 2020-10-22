import System.IO.Error
import Text.Read (readMaybe)
import Data.Char

data QA = Q QA QA String | Answer String
  deriving (Show, Read)

--Default tree
defaultQA = Q y n "Is she from Europe?"

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

-- Binds the functions together. Catches the error that occurs when there is no
-- file to read from. Instead of an error it just runs the default tree.
main :: IO ()
main = do
  putStrLn "Welcome to the Game!"
  inputFromFile <- tryIOError (readFile "./Famous.qa")
  case inputFromFile of
    Left _ -> do
      putStrLn "Couldn't read Famous.qa, using default tree instead."
      writeDefaultQA -- good idea!
      play defaultQA
    Right s -> case readMaybe s of
      Nothing -> play defaultQA
      Just qa -> play qa

-- Makes it so the game runs in a loop, with the user has a choise to continue
-- or not. If the user choose not to continue the function writes to the
-- Famous.qa file with the new and improved or at least modified question tree.
play :: QA -> IO ()
play qa = do
  qa' <- gameLoop qa
  playAgain <- yesNoQuestion "Would you like to play again?"
  case playAgain of
    True -> play qa'
    False -> do
      writeFile "./Famous.qa" (show qa')
      return ()

-- The bulk of the game, reacts accordingly to input and returns updated
-- version of the yes-branch and/or the no-branch
gameLoop :: QA -> IO QA
gameLoop (Answer name) = do
  conf <- yesNoQuestion $ "Did you think of " ++ name ++ "?"
  case conf of
    True -> do
      putStr "I Win :D "
      return (Answer name)

    False -> do
      putStrLn "OK - you win this time!"
      inPerson <- question $ "Just curious, who was it? "
      inQuestion <- question $ "Give me a question, for which the answer for "
                    ++ inPerson
                    ++ " is \"yes\" \n and the answer for "
                    ++ name
                    ++ " is \"no\""
      return (Q (Answer inPerson) (Answer name) inQuestion)

gameLoop (Q yes no q) = do
  anw <- yesNoQuestion q
  case anw of
    True -> do
      yes' <- gameLoop yes
      return (Q yes' no q)
    False -> do
      no' <- gameLoop no
      return (Q yes no' q)

question :: String -> IO String
question s = do
  putStrLn s
  a <- getLine
  return (a)

-- creates a yes or no question, returns True or False based on the answer.
yesNoQuestion :: String -> IO Bool
yesNoQuestion s = do
  reply <- question s
  ynp reply



-- if the input starts with y or Y the it accounts for yes, else no.
ynp :: String -> IO Bool
ynp ('Y':_) = return True
ynp ('y':_) = return True
ynp _ = return False
