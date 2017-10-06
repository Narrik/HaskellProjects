-- Informatics 1 Functional Programming
-- Tutorial 7
--
-- Due: 17/18 November

import System.Random


-- Importing the keymap module

import KeymapTree

-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = maximum [length (snd b)  |(a,b)<-xs]

formatLine :: Int -> (Barcode, Item) -> String
formatLine x (a,b) = a++"..."++ fst b ++ replicate (7-(length (fst b))) '.' ++ "..."++ snd b

showCatalogue :: Catalogue -> String
showCatalogue x = foldr1 (++) $ map (++"\n") (map (formatLine (longestProductLen (toList x))) (toList x))  
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe a = Just (head a)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : x) = catMaybes x
catMaybes (Just a : x) = a:catMaybes x

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems [] y = []
getItems (x:xs) y = [b |(a,b)<-(toList y),a==x] ++ getItems xs y






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
