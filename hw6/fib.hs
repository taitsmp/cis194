
module Fib where

import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- define a function that passes a tuple around.  Use the tuple to generate the next tuple.  Could we run this through a 
fibt :: (Integer, Integer) -> (Integer, Integer)
fibt (0,1) = (1,1)
fibt (x,y) = (y, x+y)

fibs2 :: [Integer]
fibs2 =  map (\a -> fst(a)) $ iterate fibt (0,1)

-- ex 3
class Stream a where
  cons :: a -> [a] -> [a]  -- how to name this fn ':'?
  cons x xs = x:xs

streamToList :: Stream a -> [a]
streamToList  (Stream a) = repeat a
