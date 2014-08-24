
module JoinList where

import Data.Monoid
import Sized
import Scrabble

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
-- consider making Append (Empty Empty) = Empty

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
     | otherwise = let j = (getSize . size $ tag j1)  
                   in if i < j then indexJ i j1 else indexJ (i-j) j2
     where 
       last' = getSize(size m) - 1 

-- could indexJ have been written with dropJ? drop n-1 then take 1 ? 

-- tested and working on inputs like
-- indexJ 2  (Append (Size 3) (Single (Size 1) 200) (Append (Size 2) (Single (Size 1) 44) (Single (Size 1) 10)))
-- indexJ 0  (Append (Size 2) (Single (Size 1) 200) (Single (Size 1) 10))

-- drop the first i elements from the joinList
-- is takeJ just the opposite of dropJ
dropJ :: (Sized b, Monoid b) => 
          Int -> JoinList b a -> JoinList b a
dropJ i = takeJ' i (<= 0) 

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ 
      | i <= 0 = Empty
takeJ i (Single m n) 
      | i > 0 = Single m n 
      | otherwise = Empty
takeJ i (Append _ j1 j2) = let k = i - tagi j1 
                           in takeJ i j1 +++ takeJ k j2

takeJ' :: (Sized b, Monoid b) =>
          Int -> (Int -> Bool) -> JoinList b a -> JoinList b a
takeJ' _ _ Empty            = Empty
takeJ' i f (Single m n)     = if f i then Single m n else Empty
takeJ' i f (Append _ j1 j2) = let k = i - tagi j1
                              in takeJ' i f j1 +++ takeJ' k f j2

scoreLine :: String -> JoinList Score String 
scoreLine s = Single (scoreString s) s

-- use 'f' on data and 'g' on annotation.
joinListFold:: b -> (b -> a -> b) -> (b -> m -> b -> b) ->  JoinList m a -> b 
joinListFold b _ _ Empty          = b
joinListFold b f _ (Single _ a)   = f b a
joinListFold b f g (Append m j1 j2) = g (joinListFold b f g j1) m (joinListFold b f g j2) 


jlFromLines:: [String] -> JoinList(Score, Size) String
jlFromLines xs = case length xs of
                   0         -> Empty
                   1         -> let s = head xs in Single (scoreString s, Size 1) s
                   _         -> let mid   = length xs `div` 2 
                                    (l,r) = splitAt mid xs
                                in jlFromLines l +++ jlFromLines r


