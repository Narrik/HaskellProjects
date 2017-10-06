{-# LANGUAGE TemplateHaskell #-}
-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 12-14 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate i xs |i<0 = "Error please select a number between 0 and length of list"
            |i>length xs = "Error please select a number between 0 and length of list"
            |otherwise = (drop i xs)++(take i xs)
     

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3.

makeKey :: Int -> [(Char, Char)] 
makeKey i |(i>26) || (i<0) = zip ['A'..'Z'] (rotate (i `mod` 26) ['A'..'Z'])
          |otherwise = zip ['A'..'Z'] (rotate i ['A'..'Z'])
-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp s cs = if [snd c | c<-cs, s == fst c]==[] then s else head[snd c | c<-cs, s == fst c]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec s [] = s
lookUpRec s (c:cs) | s==fst c = snd c
                   | otherwise = lookUpRec s cs
                   
prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp s cs = lookUp s cs==lookUpRec s cs

-- 5.
encipher :: Int -> Char -> Char
encipher i c |ord c>64 && ord c<91 = chr (65+ (mod ((ord c)+i-65) 26))
             |otherwise = c
-- 6.
normalize :: String -> String
normalize []=[]
normalize (x:xs) | isAlpha x==True = toUpper x:normalize xs
                 | isDigit x==True = x:normalize xs
                 | otherwise =normalize xs

-- 7.
encipherStr :: Int -> String -> String
encipherStr i []=[]
encipherStr i cs = encipher i (head xs):encipherStr i (tail xs) where xs=normalize cs 

-- 8. 
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xys  = [(snd xy,fst xy) | xy<-xys]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (xy:xys) = (snd xy, fst xy):reverseKeyRec xys

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xys = reverseKey xys == reverseKeyRec xys
-- 9.
decipher :: Int -> Char -> Char
decipher i c |ord c>64 && ord c<91 = chr (65+ (mod ((ord c)-i-65) 26))
             |otherwise = c

decipherStr :: Int -> String -> String
decipherStr i []=[]
decipherStr i cs = decipher i (head xs):decipherStr i (tail xs) where xs=normalize cs

-- 10.
contains :: String -> String -> Bool
contains x y = isInfixOf y x

-- 11.
candidates :: String -> [(Int, String)]
candidates xs = [(i,(decipherStr i xs))| i<-[1..25], contains (decipherStr i xs) "THE" || contains (decipherStr i xs) "AND" ]



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEach :: Int -> String -> [String]
splitEach i [] = []
splitEach i s = (take i s ++ (take (i-(length (take i s))) (cycle "X"))):splitEach i (drop i s)
splitEachFive xs = splitEach 5 xs
             

-- 13.
prop_transpose :: String -> Bool
prop_transpose s = splitEachFive s==transpose (transpose (splitEachFive s))

-- 14.
encrypt :: Int -> String -> String
encrypt i s = concat(transpose(splitEachFive (encipherStr i s))) 

-- 15.
decrypt :: Int -> String -> String
decrypt i s =decipherStr i (concat(transpose(splitEach ((length s) `div` 5) s))) 
  
-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs xs = [(x,c)|x<-['0'..'z'], let c=length(filter(==x) xs),c>0] 

-- 17
freqDecipher :: String -> [String]
freqDecipher = undefined
