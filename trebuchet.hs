import System.Environment
import Data.Char

main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let linesOfFiles = lines content
   print $ sum $ map f linesOfFiles

f :: String -> Int
f str = read [head digits, last digits]
    where digits = filter isDigit str
