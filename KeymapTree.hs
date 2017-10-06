-- INF 1 Functional Programming
-- 
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT                  
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

mytestTree :: Keymap Int Int
mytestTree = Node 5 55 (Node 4 45 (Node 3 35 (Node 2 25 (Node 1 15 Leaf Leaf) Leaf )Leaf ) Leaf) (Node 7 45 (Node 6 65 Leaf Leaf) (Node 9 95 Leaf (Node 10 105 Leaf Leaf )))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1+maximum [depth left,depth right]

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k a left right) = toList left++[(k,a)]++toList right 

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key Leaf = Nothing
get key (Node k v left right) |key == k = (Just v)
                              |key < k = get key left
                              |otherwise = get key right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList x = foldr (uncurry set) Leaf x


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT key Leaf = Leaf
filterLT key (Node k v left right) |key <= k = filterLT key left 
                                   |key > k = (Node k v (filterLT key left) (filterLT key right))

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT key Leaf = Leaf
filterGT key (Node k v left right ) | key >= k = filterGT key right
                                    | key < k = (Node k v (filterGT key left) (filterGT key right))

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf Leaf = Leaf
merge Leaf (Node k2 v2 left2 right2) = (Node k2 v2 left2 right2)
merge (Node k1 v1 left1 right1) Leaf = (Node k1 v1 left1 right1)
merge (Node k1 v1 left1 right1) (Node k2 v2 left2 right2) = merge right1(merge left1 (set k1 v1 (Node k2 v2 left2 right2)))

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del key Leaf = Leaf
del key (Node k v left right) | key == k = merge left right
                              | key < k = merge (Node k v Leaf Leaf) (merge (del key left) right)
                              | key > k = merge (Node k v Leaf Leaf) (merge left (del key right))
-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select _ Leaf = Leaf
select value (Node k v left right) | value v == False = select value (merge left right)
                                   | otherwise = merge (Node k v Leaf Leaf) (merge (select value left) (select value right))


-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary