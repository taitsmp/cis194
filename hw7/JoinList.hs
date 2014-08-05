
module JoinList where

import Data.Monoid

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
