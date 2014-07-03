
module Golf where

-- generate "nth" indexes
skipsNth :: Int -> Int -> [Int]
skipsNth len 0 = [0..(len-1)]
skipsNth len n = foldr (\i acc -> if i `rem` n == 0 then (i-1):acc  else acc) []  [1..len]
-- skipsNth len n = filter (flip (rem) n) [1..len]

-- generate indexes
skipsGI :: [a] -> [[Int]]
--skipsGI "ABCD" = [ [0,1,2,3], [1,3], [2], [3] ]
-- get the length generate numbers up to length, "length" times. you would now have a list to work with...
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
