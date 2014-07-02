
module Golf where

-- generate "nth" indexes
skipsNth :: Int -> Int -> [Int]
skipsNth len 0 = [0..(len-1)]
skipsNth len n = foldr (\i acc -> if i rem n == 0 then (i-1):acc  else acc) []  [1..len]
-- skipsNth len n = filter (flip (rem) n) [1..len]

-- generate indexes
skipsGI :: [a] -> [[Int]]
--skipsGI "ABCD" = [ [0,1,2,3], [1,3], [2], [3] ]
-- get the length generate numbers up to length, "length" times. you would now have a list to work with...
skipsGI a = let la = length a in map (skipsNth (la)) [0..(la-1)]

-- get one element of the result
skipsItem :: [x] -> [Int] -> [x]
skipsItem xs is = map (xs !! ) is --haven't tested.  might need to concat.

-- every nth element
skips :: [a] -> [[a]]
--skips "ABCD" = ["ABCD", "BD", "C", "D"]
-- skips xs = scanl (skipsItem xs) [] (gi xs)
skips xs = map (skipsItem xs) (skipsGI xs)
