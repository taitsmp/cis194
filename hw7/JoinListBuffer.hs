{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Data.Monoid
import Buffer
import JoinList

instance Buffer (JoinList(Score, Size) String) where

  -- | Convert a buffer to a String.
  toString :: JoinList(Score, Size) String -> String
  toString Empty          = []
  toString Single _ s     = s
  toString Append _ j1 j2 = [] -- not finished 

  -- | Create a buffer from a String.
  fromString :: String -> JoinList(Score, Size) String 

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line :: Int -> b -> Maybe String

--lots more to do here. 
 
