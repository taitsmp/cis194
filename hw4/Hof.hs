module Hof where

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

-- collatz-like
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 x = fun2' x

-- this is wrong.  div 2 can return an odd. 
fun2' :: Integer -> Integer
fun2' =  sum . takeWhile (>1) . iterate (`div` 2) . (\n -> if odd n then 3 * n + 1 else n)
