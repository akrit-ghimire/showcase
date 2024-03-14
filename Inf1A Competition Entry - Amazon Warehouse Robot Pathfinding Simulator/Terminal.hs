-- Akrit Ghimire 18/11/23

module Terminal where 

import Control.Concurrent (threadDelay)
import System.Process (system)

data Style = Red | Green | Yellow | Blue | Magenta | Bold | Plain
-- fmt -> format text
fmt :: Style -> String -> String
fmt style text = "\x1b[" ++ styleCode style ++ "m" ++ text ++ "\x1b[0m"
    where
        styleCode Red = "91"
        styleCode Green = "92"
        styleCode Yellow = "93"
        styleCode Blue = "94"
        styleCode Magenta = "95"
        styleCode Bold = "1"
        styleCode Plain = "97"

tellUser :: String -> String -> IO()
tellUser person message = putStrLn $ fmt Blue "[" ++ fmt Green person ++ fmt Blue "] " ++ fmt Yellow message

clearScreen :: IO ()
clearScreen = do
  _ <- system "cls"  -- Windows
--   _ <- system "clear"  -- Unix-like systems
  return ()

-- wait function in milliseconds -- used to ensure frames don't run too fast in game
wait :: Int -> IO()
wait milliseconds = threadDelay (milliseconds * 1000)