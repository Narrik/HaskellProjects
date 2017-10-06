-- Informatics 1 - Functional Programming 
-- Tutorial 6
--
-- Week 8 - Due: 10/11 Nov.


import LSystem
import Test.QuickCheck
import Data.Char

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split Sit = []
split (a :#: Sit) = split a
split (Sit :#: b) = split b
split (a :#: b) = split a ++ split b
split a = [a]
-- 1b. join
join :: [Command] -> Command
join = foldr1 (:#:)

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent a b = split a == split b

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join a = equivalent (join (split a)) a

prop_split :: Command -> Bool
prop_split a = not((contains ":#:" (show(split a)))||(Sit `elem` split a))


contains :: String -> String -> Bool
contains ys xs | map toUpper xs == map toUpper (take (length xs) ys) = True
               | ys == [] = False
               | otherwise = contains (tail ys) xs


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy i = join.concat.replicate i.split

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon x = copy 5 (Go x :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon x n = copy n (Go x :#: Turn (360/fromIntegral n))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral x n diff angle  | n>0 && x>0 = (Go x :#: Turn angle) :#: spiral (x+diff) (n-1) diff angle
                       | otherwise = Sit

-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise = reopt.removeSit.opt.removeSit

reopt :: Command -> Command
reopt a | opt(opt a) == opt a = a
        | otherwise = optimise (opt a)

removeSit :: Command -> Command
removeSit Sit = Sit
removeSit a = join (split a)

opt :: Command -> Command
opt (Go 0) = Sit
opt (Go 0 :#: a) = opt a

opt (Turn 0) = Sit
opt (Turn 0 :#: a) = opt a

opt (Go x) = if x>0 then (Go x) else error "negative DistanceB"
opt (Go x :#: Go y) = if (x>0) && (y>0) then opt (Go (x+y)) else error "negative DistanceB" 
opt (Go x :#: Go y :#: a) = if (x>0) && (y>0) then opt (Go (x+y) :#: a) else error "negative DistanceB"

    
opt (Turn x :#: Turn y) = opt (Turn (x+y))
opt (Turn x :#: Turn y :#: a) = opt (Turn (x+y) :#: a)

opt (a :#: b) = a :#: opt b
opt a = a



-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x  = f x 
 where
 f 0 = GrabPen red :#: Go 10
 f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
 g 0 = GrabPen blue :#: Go 10
 g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
 p = Turn (-60)
 n = Turn 60


-- 6. snowflake
snowflake :: Int -> Command
snowflake x  = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
 where 
 f 0 = GrabPen blue :#: Go 10 
 f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
 p = Turn (-60)
 n = Turn 60


-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

tree :: Int -> Command
tree x = f x
	where
f 0= GrabPen red :#: Go 10
f x= g (x-1) :#: Branch (n :#: f (x-1)) :#: Branch (p :#: f (x-1)) :#: Branch (g (x-1) :#: f (x-1))
g 0 = GrabPen blue :#: Go 10
g x = g (x-1) :#: g (x-1)
n = Turn 45
p = Turn (-45)
