module Hof where

import Data.List

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

-- collatz-like
fun2 :: Integer -> Integer
fun2 =  sum . filter even . takeWhile (>1) . iterate (\n -> if odd n then 3 * n + 1 else n `div` 2)


data Tree a = Leaf
             | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

-- keep tree balanced.  Always insert node into shortest path. 
-- accumulator keeps track of new tree state. 
-- use foldr so the last element is inserted first. 
-- note: I think this is easier than I'd think.  Shortest path and longest path can still differ by two and trees can be balanced. 
foldTree :: [a] -> Tree a
foldTree xs = let len = length xs in
                  foldr (treeInsert) Leaf $ zipWith (\h a -> (h,a)) (heights len) xs


treeInsert :: (Integer, a) -> Tree a -> Tree a
treeInsert (h,x) Leaf = (Node h Leaf x Leaf)
treeInsert tup (Node ht t1 y t2) 
   | d1 < d2 = (Node ht (treeInsert tup t1) y t2) 
   | otherwise = (Node ht t1 y (treeInsert tup t2))
   where d1 = depthOfClosestLeaf 0 t1
         d2 = depthOfClosestLeaf 0 t2

-- if this function is wrong replace it with a function that counts leaves in a tree.  
-- subtree with fewest leaves is one way to determine where to insert (after comparing subtree heights). 
depthOfClosestLeaf :: Int -> Tree a -> Int 
depthOfClosestLeaf i Leaf = i
depthOfClosestLeaf i (Node _ t1 _ t2) = min d1 d2 
                                          where d1 = depthOfClosestLeaf (i+1) t1 
                                                d2 = depthOfClosestLeaf (i+1) t2


-- idea: you know the number of items to insert.  calculate height of each item before you insert it. 
heights :: Int -> [Integer]
heights i = reverse . map toInteger . take i $ heights' h h
            where h = (ceiling . toRational . logBase 2 $ fromIntegral i) - 1 


--this is wrong... heights' 1 1
heights' :: Int -> Int -> [Int] -- start and num items in list?
heights' start 0 = let nodes = 2^start in replicate nodes 0
heights' start dep = let nodes = (2^(start - dep)) in (replicate nodes dep) ++ (heights' start (dep-1)) 


-- not part of hw.  Take Tree and fold it to something else
treefold :: b -> (Tree a -> b -> b) -> Tree a -> b
treefold z _ Leaf = z
treefold z f (Node _ t1 y t2) = f t1 (treefold z f t2)  
treefold z f (Node _ t1 y t2) = f t1 (treefold (f t2 (treefold z f (Node 0 Leaf y Leaf))) f Leaf ) 

-- implement xor as a fold
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not acc else acc) False 

-- implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x):acc) [] 

-- given n generate all primes up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let m = 2*n+2 in takeWhile (<m) . map (\k -> 2*k+1) $ step1 n

-- generate the numbers to remove from the list first. 
step1 :: Integer -> [Integer]
step1 n = [1..n] \\ (map (\(a,b) -> a+b+2*a*b ) $ cartProd [1..n] [1..n])

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys] 

