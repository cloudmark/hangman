module Main where

import           Control.Monad (forever, when)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

maxWrong :: Int
maxWrong = 10

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = (WordList . lines) <$> readFile "data/dict.txt"


gameWords :: IO WordList
gameWords = do
  WordList aw <- allWords
  return (WordList (filter gameLength aw))
  where gameLength w =
          let l = length (w :: String)
          in  l >= minWordLength
              && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] String Int
instance Show Puzzle where
  show (Puzzle _ discovered guessed invalid) =
    intersperse ' ' (renderPuzzleChar <$> discovered) ++
      " Guessed so far: " ++ guessed ++ " [" ++ show (maxWrong - invalid)  ++" attemps remaining.]"

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just x) = x
renderPuzzleChar Nothing  = '_'

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (fmap (const Nothing) word) "" 0

charInWord :: Puzzle -> Char ->  Bool
charInWord (Puzzle word _ _ _) e = e `elem ` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) e = e `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s invalid) c =
    Puzzle word newFilledInSoFar (c:s) newInvalid
      where newInvalid = if countJust newFilledInSoFar == countJust filledInSoFar then invalid + 1 else invalid

            newFilledInSoFar :: [Maybe Char]
            newFilledInSoFar = zipWith (zipper c) word filledInSoFar

            zipper :: Char -> Char -> Maybe Char -> Maybe Char
            zipper guess wordChar guessChar
                | wordChar == guess = Just wordChar
                | otherwise = guessChar

            countJust :: [Maybe Char] -> Int
            countJust = length . filter (/= Nothing)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> putStrLn "You already guess that character, pick something else!" >> return puzzle
    (True, _) ->  putStrLn "This character was in the word, filling in the word accordingly" >> return (fillInCharacter puzzle guess)
    (False,_) -> putStrLn "This character was not in the word, try again! " >> return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed invalids) =
  when (invalids > maxWrong) $
      do
        putStrLn "You lose!"
        putStrLn ("The word was " ++ wordToGuess)
        exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  when (all isJust filledInSoFar) $
      putStrLn "You win!" >> exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn ("Current Puzzle is: " ++ show puzzle)
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
