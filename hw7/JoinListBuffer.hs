{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Data.Monoid
import Buffer
import JoinList

instance Buffer (JoinList(Score, Size) String) where

  -- | Convert a buffer to a String.
  toString :: JoinList(Score, Size) String -> String
  toString j = joinListFold [] (++) id  

  -- | Create a buffer from a String.
  fromString :: String -> JoinList(Score, Size) String 
  fromStrng s = let sc = scoreString s
                    sz = length s 
                in Single (sc, Size sz) s

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line :: Int -> b -> Maybe String

   -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine :: Int -> String -> b -> b

  -- | Compute the number of lines in the buffer.
  numLines :: b -> Int

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value :: b -> Int
