module Hof where

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
-- need helper function to insert node into tree?  just use it when need to return accumulator? given node and tree return a new tree? could just fold on that fn?
-- how to do breadth first search for "leaf" for insert? Ensure only one leaf found and one insert made.
-- pass calculated distance from root to leaf via recursion. leaf with smallest dist is where we'd want to insert. 
-- update all heights after insert(?) update height as you follow path? 
foldTree :: [a] -> Tree a
foldTree xs = foldr (treeInsert) Leaf xs

treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf = (Node 0 Leaf x Leaf)
treeInsert x (Node i t1 _ t2) 
   | d1 < d2 = treeInsert x t1 -- wish I could just update the height here but I can't...would be wrong and vars are immutable.  
   | otherwise = treeInsert x t2 
   where d1 = depthOfClosestLeaf 0 t1
         d2 = depthOfClosestLeaf 0 t2

depthOfClosestLeaf :: Int -> Tree a -> Int 
depthOfClosestLeaf i Leaf = i
depthOfClosestLeaf i (Node h t1 _ t2) = min d1 d2 
                                          where d1 = depthOfClosestLeaf (i+1) t1 
                                                d2 = depthOfClosestLeaf (i+1) t2

-- when to call this?  height will be inaccurate right after an insert.  
-- treeHeight :: Tree a -> Int
-- treeHeight Leaf = 0
-- treeHeight (Node i t1 a t2) = (max h1 h2) + 1 where
