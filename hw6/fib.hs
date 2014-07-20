
module Fib where

import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- come up with O(n) recursive definition of fibs
-- fibs2
--- not lazy or correct.
fibs2' :: [Integer]
fibs2'= foldl' (\acc@(x1:x2:_) _ -> let y = (x1+x2) in y:acc ) [0,1] [0..]

-- could I define a bunch of partial functions that get mapped over.
fib' :: Integer -> Integer -> Integer
fib' [] = [0]
fib' [0,1] = [0,1,1]
fib' all@(x:y:_) = o

-- define a function that passes a tuple around.  Use the tuple to generate the next tuple.  Could we run this through a 
fibt :: (Integer, Integer) -> (Integer, Integer)
fibt (0,1) = (1,1)
fibt (x,y) = (y, x+y)

fibs2 :: 
fibs2 =  iterate fibt (0,1)

-- ex 3
class Stream a where
  cons :: a -> [a] -> [a]  -- how to name this fn ':'?

