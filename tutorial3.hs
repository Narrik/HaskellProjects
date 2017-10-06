-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 19-21 Oct.

import Data.Char
import Test.QuickCheck
import Data.List


-- 1. Map
-- a.
uppers :: String -> String
uppers = map toUpper 

-- b.
doubles :: [Int] -> [Int]
doubles = map (*2)

-- c.        c
penceToPounds :: [Int] -> [Float]
penceToPounds = map ((/100).fromIntegral) 

-- d.
uppers' :: String -> String
uppers' xs = [toUpper x | x<-xs]

prop_uppers :: String -> Bool
prop_uppers xs = uppers xs == uppers' xs



-- 2. Filter
-- a.
alphas :: String -> String
alphas = filter isAlpha

-- b.
rmChar ::  Char -> String -> String
rmChar c = filter (/=c)

-- c.
above :: Int -> [Int] -> [Int]
above i  = filter (>i)

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter (\(x,y)->x==y)

-- e.
rmCharComp :: Char -> String -> String
rmCharComp c cs = [s | s<-cs,c/=s] 

prop_rmChar :: Char -> String -> Bool
prop_rmChar c cs = rmChar c cs==rmCharComp c cs



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' xs = map toUpper (filter (isAlpha) xs)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs  = map (*2) (filter (>3) xs) 

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' xs = map (reverse) (filter (\s -> even (length s)) xs)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold = foldr (*) 1

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec []=True
andRec (x:xs) |x==True = andRec xs
              |otherwise = False

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [[a]] -> [a]
concatRec []=[]
concatRec (x:xs) =  x++(concatRec xs)

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] xs = xs
rmCharsRec (c:cs) xs = rmChar c (rmCharsRec cs xs)

rmCharsFold :: String -> String -> String
rmCharsFold cs xs = foldr rmChar xs cs

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform xs = foldr (==) True (map (== head xs) xs)
        
-- b.
valid :: Matrix -> Bool
--valid xs = uniform [length x | x<-xs] == True && [null x | x<-xs] == replicate (length [null x | x<-xs]) False && null xs==False
valid xs |uniform [length x | x<-xs] == False = False
         |length xs <=0 = False
         |length (head xs) <= 0 = False
         |otherwise = True
        
-- 6.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys =[f x y |(x,y)<-zip xs ys]

--c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)
 
-- 7.
plusM :: Matrix -> Matrix -> Matrix
plusM as bs = zipWith plusRow as bs

plusRow :: [Int]->[Int]->[Int]
plusRow xs ys = zipWith (+) xs ys

-- 8.
timesM :: Matrix -> Matrix -> Matrix
timesM xs ys | length (head(transpose xs)) /= length (head ys)  = error  "Unable to compute a multiplication of such matrices"
             | otherwise = [map (dot x) (transpose ys)| x<-xs]

dot :: [Int] -> [Int] -> Int
dot xs ys = foldr (+) 0 (zipWith (*) xs ys)
-- Optional material
-- 9.

vowelly :: Int -> Bool
vowelly x = elem x [1,8,11,18] || elem x  [80..89]

count :: [Int] -> Int
count xs = length [x | x<-xs , vowelly x]

countRec :: [Int] -> Int
countRec [] = 0
countRec (x:xs) | vowelly x = 1+countRec xs
                | otherwise = countRec xs

c :: Char -> String -> String
c k xs = [(if even y then k else x ) | (x,y) <-zip xs [0..]]

d :: Char -> String -> String
d k [] = []
d k [x] = [k]
d k (x:y:xs) = k:y:d k xs

prop_cd :: Char -> String -> Bool
prop_cd k xs = c k xs == d k xs
