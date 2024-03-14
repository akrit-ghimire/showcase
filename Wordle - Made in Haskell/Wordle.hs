-- Wordle is a 5 letter word guessing game
-- If you get a letter that is in the word then it is yellow
-- If you get a letter that is in the word in the right place it becomes green
-- The aim is to guess the word before you run out of tries

import qualified Data.Map as Map
import Data.Char (toUpper)
import System.IO (readFile)
import WordleFunctions (isInWordList, drawGame, getRandomElement, generateWordPatternMap, validateWord, generateWordRow, splitString, fmt, FontCode(Red, Green, Yellow))

gameLoop :: Int -> [(String, [Int])] -> String -> Map.Map Char Int -> String -> [String] -> String -> IO()
gameLoop tries gameMatrix correctWord correctWordPattern error wordList lastWord
    | lastWord == correctWord = do 
        drawGame gameMatrix correctWord correctWordPattern
        putStrLn $ fmt Green "\nCONGRATS! YOU GOT THE WORD IN " ++ fmt Yellow (show (5-tries)) ++ fmt Green " TRIE(S)\n"
    | tries == 0 = do
        drawGame gameMatrix correctWord correctWordPattern
        putStrLn $ fmt Red "\nYOU FAILED TO GUESS THE WORD. THE WORD WAS " ++ fmt Yellow correctWord ++ "!\n"
    | otherwise
    = do
        drawGame gameMatrix correctWord correctWordPattern

        putStrLn error
        putStrLn "Enter a 5 letter word."
        userInput <- getLine

        let userWord = [ toUpper c | c <- userInput] -- to uppercase
        
        if length userWord == 5 && isInWordList userWord wordList then do 
            let userValidated = validateWord userWord correctWord correctWordPattern [] 0
                newMatrix = gameMatrix ++ [(userWord, userValidated)]
            gameLoop (tries-1) newMatrix correctWord correctWordPattern "" wordList userWord
        else do
            let error = "Word must be 5 letters long and in Word List"
            gameLoop tries gameMatrix correctWord correctWordPattern error wordList userWord
        
main :: IO ()
main = do
    loadedWordList <- readFile "WordList.txt"
    let formattedWordList = splitString loadedWordList
    correctWord <- getRandomElement formattedWordList
    let correctWordPattern = generateWordPatternMap correctWord Map.empty

    gameLoop 5 [] correctWord correctWordPattern "" formattedWordList "     "


    -- to do
    -- // get input and validate whether it is a 5 letter word that is a word
    -- // assess algorithm to validae the yellows and greens of each inputed word
    -- // create game matrix function that shows blanks until all 5 attempts made
    -- // export wordlist to external file and pull in data
    -- add dynamic padding so game is in centre of console + cls + refresh each time attempt made
