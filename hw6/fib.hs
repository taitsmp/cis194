
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
data Stream a = Element a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Element x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show a = foldl (\acc x ->  acc ++ show x ) "" (take 20 $ streamToList a)

-- ex4
streamRepeat :: a -> Stream a
streamRepeat a = Element a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Element x s) = Element (f x) (streamMap f s) 

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = let y = f x in
                     Element y (streamFromSeed f y) 

