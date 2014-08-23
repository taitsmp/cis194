{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Data.Monoid
import Buffer
import JoinList
import Sized
import Scrabble

instance Buffer (JoinList(Score, Size) String) where

  -- | Convert a buffer to a String.
  toString = joinListFold [] (++) (\s1 _ s2 -> s1 ++ s2)

  -- | Create a buffer from a String.
  fromString s = jlFromLines $ lines s

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line :: Int -> JoinList(Score, Size) String  -> Maybe String
  line i = Nothing
  --use indexJ to implement this.  

   -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine :: Int -> String -> JoinList(Score, Size) String  -> JoinList(Score, Size) String 
  -- hardest function to write. 

  -- | Compute the number of lines in the buffer.
  numLines :: JoinList(Score, Size) String  -> Int
  -- top element should have this in its' Size monoid

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value :: JoinList(Score, Size) String  -> Int
  -- top element should have this it its' Score monoid


--

