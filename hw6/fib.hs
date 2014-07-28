
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
  show a = foldl (\acc x ->  acc ++ " " ++ show x ) "" (take 20 $ streamToList a)

-- ex4
streamRepeat :: a -> Stream a
streamRepeat a = Element a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Element x s) = Element (f x) (streamMap f s) 

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = let y = f x in
                     Element y (streamFromSeed f y) 

-- ex5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Element e1 s1) s2 = Element e1 (interleaveStreams s2 s1)
--interleaveStreams (Element e1 s1) (Element e2 s2) = Element e1 (Element e2 (interleaveStreams s1 s2)) -- defining like this will cause streams to recursive infinitely
-- http://www.haskell.org/pipermail/beginners/2014-February/013160.html
--interleaveStreams (Element e1 s1) ~(Element e2 s2) = Element e1 (Element e2 (interleaveStreams s1 s2))  -- use of irrefutable pattern 

istreams :: Integer -> Stream Integer
istreams n = interleaveStreams (streamRepeat n) (istreams (n+1))

--the solution below is correct but obviously hard coded. 
--interleaveStreams (streamRepeat 0) (interleaveStreams (streamRepeat 1) (interleaveStreams (streamRepeat 2) (streamRepeat 3)))

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])
--ruler = istreams 0
