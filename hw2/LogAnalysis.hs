{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where 

import Log

-- extract ints from "words" in a message
messageInts :: [String] -> [Int]
messageInts wds = foldl (\acc x -> let i = reads x :: [(Int, String)] in if null i then acc else ((fst(head i)) ::Int):acc) [] wds

parseMessage :: String -> LogMessage
parseMessage (t:' ':xs)
              | t == 'I' =  (LogMessage Info p1 txt)
              | t == 'E' =  (LogMessage (Error p1) (read (wds !! 1) :: Int) (unwords . tail $ tail wds))
              | otherwise = Unknown (t:' ':xs)  -- do we need this?
              where wds = words xs
                    p1  = read (head wds) :: Int
                    txt = unwords $ tail wds
parseMessage msg = Unknown msg

parse :: String -> [LogMessage]
parse f = map (parseMessage) $ lines f


