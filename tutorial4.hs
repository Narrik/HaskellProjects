-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (27/28 Oct)

import Data.List (nub,isPrefixOf,intercalate)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString xs ys = if length xs == length ys then and[(toUpper x == toUpper y) | (x,y)<- zip xs ys] else False


-- 2.
prefix :: String -> String -> Bool
prefix xs ys = if length xs <= length ys then and[(toUpper x == toUpper y) | (x,y)<- zip xs ys] else False 

prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
             prefix substr (map toUpper str)
                           where
                             substr  =  take n str

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str
        
        
-- 3.
contains :: String -> String -> Bool
contains ys xs | map toUpper xs == map toUpper (take (length xs) ys) = True
               | ys == [] = False
               | otherwise = contains (tail ys) xs



prop_contains :: String -> Int -> Int -> Bool
prop_contains xs d t = contains xs (take t (drop d xs))


-- 4.
takeUntil :: String -> String -> String
takeUntil xs ys  | map toUpper xs == map toUpper (take (length xs) ys) = []
                 | ys == [] = []
                 | otherwise = head ys : takeUntil xs (tail ys)

dropUntil :: String -> String -> String
dropUntil xs ys  | map toUpper xs == map toUpper (take (length xs) ys) = drop (length xs) ys
                 | ys == [] = []
                 | otherwise = dropUntil xs (tail ys)


-- 5.
split :: String -> String -> [String]
split [] ys = error "empty string for separator"
split xs [] = [""]
split xs ys | takeUntil xs ys == ys = [ys]
            | otherwise = takeUntil xs ys:split xs (dropUntil xs ys)

reconstruct :: String -> [String] -> String
reconstruct xs [] = []
reconstruct xs ys = concat(map (++xs) (init ys))++last ys

reconstruct' :: String -> [String] -> String
reconstruct' xs [] = []
reconstruct' xs ys = intercalate xs ys

prop_rec :: String -> [String] -> Bool
prop_rec xs ys = reconstruct xs ys == reconstruct' xs ys
 

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML xs = tail(split "href=\"" xs)
testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails xs = [x | x<-xs, prefix "mailTo" x]


-- 8.
link2pair :: Link -> (Name, Email)
link2pair x | prefix "MAILTO" x = (takeUntil "</a>" (dropUntil ">" x),dropUntil "mailto:"(takeUntil "\"" x))
            | otherwise = error "not an e-mail adress"


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML xs = nub(map link2pair (takeEmails (linksFromHTML xs)))

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail x ys = [(y1,y2) | (y1,y2)<-ys, contains y1 x]


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML xs y = findEmail y (emailsFromHTML xs)


-- Optional Material

-- 12.
hasInitials :: String -> Name -> Bool
hasInitials = undefined

-- 13.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML = undefined

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML = undefined

-- 14.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: Name -> Bool
myCriteria = undefined

emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML = undefined

-- 15
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
