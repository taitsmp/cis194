
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
     | otherwise = case (tag j1, tag j2) of 
                                           (k, j)      -> let ki = getSize(size k)
                                                              ji = getSize(size j) 
                                                          in if i < ki then indexJ ji j1 else indexJ (ki - i) j2
                                                             
     where 
       last' = getSize(size m) - 1 
