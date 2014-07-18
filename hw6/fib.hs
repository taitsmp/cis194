
module Fib where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = iterate (\n -> fib (n+1)) 0 -- can we use iterate for fibs1?  Probably not.  Will just return zeros. 



