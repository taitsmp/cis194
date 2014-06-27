
import Data.Char
import Data.List.Split

toDigits :: Integer -> [Integer]
toDigits x
         | x <= 0    = []
         | otherwise = map (toInteger . digitToInt) $ show x
--toDigits d = foldr (\a acc -> (toInteger $ digitToInt a):acc) [] $ show d
-- map (\x -> read [x]::Int) "1234"

toDigitsRev :: Integer -> [Integer]
toDigitsRev xs = reverse $ toDigits xs

--double 2nd to last, 4th to last, etc.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse . concat . map (\[x,y] -> [x, 2*y]). chunksOf 2 $ reverse xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum . concat $ map toDigits xs

validate :: Integer -> Bool
validate x = (0 == (sumDigits . doubleEveryOther $ toDigits x))
