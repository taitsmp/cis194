
module Golf where

import Data.List

-- generate "nth" indexes
skipsNth :: Int -> Int -> [Int]
skipsNth len 0 = [0..(len-1)]
skipsNth len n = foldr (\i acc -> if i `rem` n == 0 then (i-1):acc  else acc) []  [1..len]

-- generate indexes
skipsGI :: [a] -> [[Int]]
skipsGI a = let la = length a in map (skipsNth la) [1..la]

-- get one element of the result
skipsItem :: [x] -> [Int] -> [x]
skipsItem xs is = map (xs !! ) is --haven't tested.  might need to concat.

-- every nth element
skips :: [a] -> [[a]]
skips xs = map (skipsItem xs) (skipsGI xs)

midLarge :: Int -> Int -> Int -> Bool
midLarge a b c = b > a && b > c

genTriples :: [Int] -> [[Int]]
genTriples (a:b:c:[]) = [[a,b,c]]
genTriples xs = take 3 xs : genTriples (tail xs)

-- localMaxima = has 
localMaxima :: [Int] -> [Int]
-- generate all lists of 3 items or more
localMaxima is = foldl (\acc (a:b:c:_) -> if midLarge a b c then b:acc else acc ) [] $ genTriples is

data HistLine = HistLine HistDots HistMore
type HistMore = [Int]
type HistDots = [Int]

-- TODO: can we use a data type for the return type?  
-- hdistinct :: [Int] -> [[Int]]
hdistinct :: [Int] -> HistLine
hdistinct is = let u = nub is in HistLine u (is \\ u) 

hprintLine :: HistDots -> String
hprintLine is = foldr (\x acc -> if x `elem` is then '*':acc else ' ':acc) [] [1..9]

hkey :: String
hkey = replicate 9 '=' ++ "\n123456789\n" 

histogram :: [Int] -> String
histogram is = histogram' is ++ "\n" ++ hkey

-- print as you go. remove items from the list. stop printing new lines once the list is finished. 
-- 
histogram' :: [Int] -> String
histogram' is = let HistLine dots more = hdistinct is in 
                   case more of 
                      [] -> hprintLine dots
                      _  -> histogram' more ++ "\n" ++ hprintLine dots

