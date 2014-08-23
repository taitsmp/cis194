{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import Data.Monoid
import Buffer
import JoinList
import Sized
import Scrabble

instance Buffer (JoinList(Score, Size) String) where

  -- | Convert a buffer to a String.
  toString :: JoinList(Score, Size) String -> String
  toString j = joinListFold [] (++) id  


  -- would be better to create a tree structure one 'Single' per line?
  -- how to insert and keep lines in order?
  -- how to insert lines and keep the tree somewhat balanced?
  -- could I make a list with tuples of line number and JoinList.  randomize the list and then insert into the tree. won't be perfectly balanced but won't be terrible either
     -- won't work..can't tell ordering. 
  -- can I use 'size' to know where to insert?  smallest subtree gets the element.  won't work
  -- can I use indexJ at all to help?
  -- lines only stored on "single"
  -- 1. create Single for each line.  
  -- 2. Take list of Singles and insert into JoinList.
  -- | Create a buffer from a String.
  fromString :: String -> JoinList(Score, Size) String 
  fromString s = ss = map (lines) s


let sc = scoreString s

                    sz = length s 
                in Single (sc, Size sz) s

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

