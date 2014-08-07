
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

indexJ :: (Sized b, Monoid b) => 
          Int -> JoinList a b -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a) 
     | i == 0    = Just a
     | otherwise = Nothing
indexJ i (Append m j1 j2) 
     | i > last  = Nothing
     | i < 0     = Nothing 
     | otherwise = case (tag(j1), tag(j2)) of 
                                           (mempty, _) -> indexJ i j2
                                           (_, mempty) -> indexJ i j1
                                           (k, j)      -> let ki = getSize(k)
                                                              ji = getSize(j) 
                                                          in if i < ki then indexJ i j1 else indexJ (ki - i) j2
                                                             
     where 
       last = getSize(m) - 1 
