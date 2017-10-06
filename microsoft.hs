
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Text

main :: IO ()
main = do
  code <- getLine
  msg <- getLine
  print (decoded code msg)

decoded :: String -> String -> String
decoded code [] = []
decoded code (y:ys) = (if isAlpha y == False then y else findout xs y) : decoded code ys
 where xs = makeList code

findout :: [String] -> Char -> Char
findout (x:xs) y = if toUpper y == head x then (if y == toUpper y then last x else toLower (last x)) else findout xs y


makeList :: String -> [String]
makeList [] = []
makeList x = [takeWhile (/=' ') x] ++ (if dropWhile (/=' ') x /= [] then makeList (tail (dropWhile (/=' ') x)) else [])
