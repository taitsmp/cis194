{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Buffer
import JoinList
import Sized
import Scrabble

instance Buffer (JoinList(Score, Size) String) where

  -- | Convert a buffer to a String.
  toString = joinListFold [] (++) (\s1 _ s2 -> s1 ++ s2)

  -- | Create a buffer from a String.
  fromString = jlFromLines . lines 

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line i jl = indexJ i jl

   -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine i s j = takeJ (i-1) j +++ Single (scoreString s, Size 1) s +++ dropJ i j

  -- | Compute the number of lines in the buffer.
  numLines jl = tagi jl 

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value jl = let (sc, _) = tag jl in getScore sc

--

