module WordleFunctions where

import qualified Data.Map as Map
import System.Process (system)
import System.Random (randomRIO)
import Data.Char (toUpper)

clearConsole :: IO ()
clearConsole = do
  _ <- system "cls"  -- Windows
--   _ <- system "clear"  -- Unix-like systems
  return ()

generateWordPatternMap :: String -> Map.Map Char Int -> Map.Map Char Int
generateWordPatternMap word accumulatedPattern = 
    -- "SLEEP" Map.Map -> [("S", 1), ("L", 1), ("E", 2), ("P", 1)]
    if null word then accumulatedPattern 
    else let letter = head word
             updatedPattern = Map.insertWith (+) letter 1 accumulatedPattern
             -- if item exists, adds 1 to it. If it doesn't it creates the item and adds 1 to it
             in generateWordPatternMap (tail word) updatedPattern

validateWord :: String -> String -> Map.Map Char Int -> [Int] -> Int -> [Int]
validateWord word correctWord correctWordPattern accumulatedValidation idx = if idx > length word -1 then accumulatedValidation else do
    -- u"SLOPS" [("S", 2), ("L", 1), ("O", 1), ("P", 1)] --> [2, 2, 0, 1, 0]
    -- c"SLEEP" [("S", 1), ("L", 1), ("E", 2), ("P", 1)] --> 2 = right place ; 1 = right letter wrong place ; 0 = wrong
    let uLetter = word !! idx
        cLetter = correctWord !! idx
        rightPlace = uLetter == cLetter
        frequency = Map.findWithDefault 0 uLetter correctWordPattern

    -- if found "s" then remove it from word pattern to prevent multiple "s"'s getting highlighted
    -- else letter was not in word so ammend 0 to the list
    -- for debugging print $ show frequency ++ " " ++ show uLetter ++ " " ++ show correctWordPattern ++ " " ++ show accumulatedValidation
    if frequency > 0
        then 
            let updatedWordPattern = Map.insertWith (+) uLetter (-1) correctWordPattern
                updatedValidation = if rightPlace then accumulatedValidation ++ [2] else accumulatedValidation ++ [1]
            in validateWord word correctWord updatedWordPattern updatedValidation (idx+1)
        else let updatedValidation = accumulatedValidation ++ [0] in validateWord word correctWord correctWordPattern updatedValidation (idx+1)

isInWordList :: String -> [String] -> Bool
isInWordList word wordList = word `elem` wordList

getRandomElement :: [a] -> IO a
getRandomElement list = (list !!) `fmap` randomRIO (0, length list - 1)

-- format text types
data FontCode = Red | Green | Yellow | Blue | Magenta | Bold | Plain
-- fmt -> format text
fmt :: FontCode -> String -> String
fmt color text = "\x1b[" ++ fontCode color ++ "m" ++ text ++ "\x1b[0m"
    where
        fontCode Red = "91"
        fontCode Green = "92"
        fontCode Yellow = "93"
        fontCode Blue = "94"
        fontCode Magenta = "95"
        fontCode Bold = "1"
        fontCode Plain = "97"

generateLetterBox :: Char -> String
generateLetterBox l = " " ++ l : " "

generateDash :: Int -> IO()
generateDash rep = if rep == 0 then putStr "\n" else do 
    putStr "-"
    generateDash (rep-1)

generateWordRow :: Int -> String -> [Int] -> IO()
generateWordRow curr word status = if curr == 0 then putStrLn "|" else do
    -- 0 = No match ; 1 = Match letter but wrong place ; 2 = Match letter
    let idx = 5-curr
        letter = word !! idx
        letterStatus = status !! idx

    if letterStatus == 0 then putStr $ "|" ++ fmt Plain (generateLetterBox letter)
    else if letterStatus == 1 then putStr $ "|" ++ fmt Yellow (generateLetterBox letter)
    else putStr $ "|" ++ fmt Green (generateLetterBox letter)

    generateWordRow (curr-1) word status

formatGameMatrix :: [(String, [Int])] -> [(String, [Int])]
-- fills an empty matrix to the required 5 length
-- i.e ["HELLO", "SLOPS"] -> ["HELLO", "SLOPS", "     ", "     ", "     "]
formatGameMatrix matrix = if length matrix == 5 then matrix else matrix ++ [("     ", [0,0,0,0,0]) | _ <- [1..(5-length matrix)]]

drawGrid :: [(String, [Int])] -> Int -> String -> Map.Map Char Int -> IO()
drawGrid gameMatrix idx correctWord correctWordPattern = if idx > length gameMatrix -1 then generateDash 21 else do
    let (word, validation) = gameMatrix !! idx
    generateDash 21
    generateWordRow 5 word validation
    drawGrid gameMatrix (idx+1) correctWord correctWordPattern

drawGame :: [(String, [Int])] -> String -> Map.Map Char Int -> IO()
drawGame gameMatrix correctWord correctWordPattern = do
    clearConsole
    putStrLn $ fmt Bold "Wordle\n"
    drawGrid (formatGameMatrix gameMatrix) 0 correctWord correctWordPattern

splitString :: String -> [String]
splitString commaListString = 
    let (word, rest) = break (== '\n') commaListString
        in
    if null rest then [[ toUpper c | c <- word]]
    else [ toUpper c | c <- word] : splitString (tail rest)