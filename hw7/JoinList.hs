
module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m 
tag Empty = mempty
tag (Append m _ _) = m

tagi :: (Sized m, Monoid m) => JoinList m a -> Int
tagi j = getSize . size $ tag j

-- get the tag on two join lists then perform mappend between the two to create the new annotation. 
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a `mappend` tag b) a b

-- helper function.  given the moniod 


-- return element 'a' at the provided index from the joinlist b a.
indexJ :: (Sized b, Monoid b) => 
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ x) 
     | i == 0    = Just x
     | otherwise = Nothing
indexJ i (Append m j1 j2) 
     | i > last' = Nothing
     | i < 0     = Nothing 
     | otherwise = let j = getSize . size $ tag j1
                       k = getSize . size $ tag j2
                   in if i < j then indexJ i j1 else indexJ (i-j) j2
                                                             
     where 
       last' = getSize(size m) - 1 

-- another way to do indexJ
-- 1. use +++ to create a JoinList of only the nodes on the path to the element we want (always use right child?). track the number of moves to get to the element.  this is i.
-- 2. write a function to grab an index from a JoinList of right only
-- dumb. once you've walked the path to find element why build up another list.  not sure how to use +++ or a function like ||? to implement this.

-- drop the first i elements from the joinList
dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i (Single m n) 
      | i <=0 = (Single m n)
      | otherwise = Empty
dropJ i (Append _ j1 j2) = let p = (tagi j1) - i
                               q = i - (tagi j2) 
                           in (dropJ p j1) +++ (dropJ q j2)



