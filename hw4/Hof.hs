module Hof where

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

-- fun2 :: [Integer] -> Integer
-- fun2 = 
