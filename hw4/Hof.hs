module Hof where

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

-- collatz-like
fun2 :: Integer -> Integer
fun2 =  sum . filter even . takeWhile (>1) . iterate (\n -> if odd n then 3 * n + 1 else n `div` 2)
