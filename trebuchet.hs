import System.Environment
import Data.Char
import Data.List
import Data.Maybe

main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let linesOfFiles = lines content
   print $ "part 1: " ++ show(sum $ map parseLine1 linesOfFiles)
   print $ "part 2: " ++ show(sum $ map parseLine2 linesOfFiles)

--part 1
parseLine1 :: String -> Int
parseLine1 str = read [head digits, last digits]
   where digits = filter isDigit str

--part 2
parseLine2 :: String -> Int
parseLine2 str = (head digits) * 10 + (last digits)
   where digits = catMaybes $ map parse $ tails str

digitNames = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

parse :: String -> Maybe Int
parse [] = Nothing
parse x@(c:_)
   | isDigit c = Just $ read [c] 
   | Just t <- find (flip(isPrefixOf) x . fst) $ zip digitNames [1..]  = Just $ snd t
   | otherwise = Nothing
