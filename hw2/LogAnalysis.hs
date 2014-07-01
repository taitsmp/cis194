--{-# OPTIONS_GHC -Wall #-}
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

-- insert new LogMessage into sorted MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = (Node Leaf lm Leaf) 
insert lm1 (Node t1 lm2 t2) = 
	case (lm1, lm2) of 
		((LogMessage _ ts1 _), (LogMessage _ ts2 _)) ->  if ts1 <  ts2 
			then (Node (insert lm1 t1) lm2 t2) 
			else (Node t1 lm2 (insert lm1 t2))
-- is there a cleaner way to do the above?  

build :: [LogMessage] -> MessageTree
build lms = foldl (\mt lm -> insert lm mt) Leaf lms

-- traverse a message tree in order and create a list of LogMessages
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
--inOrder (Node Leaf lm mt) = lm : (inOrder mt)
--inOrder (Node mt lm Leaf) = (inOrder mt) ++ [lm]
inOrder (Node mt1 lm mt2) = (inOrder mt1) ++ [lm] ++ (inOrder mt2)

-- experimenting with "getter" functions
errorTimeStamp :: LogMessage -> Maybe TimeStamp 
errorTimeStamp lm = case lm of 
                     (LogMessage (Error _ ) ts _) -> Just ts
                     _ -> Nothing

-- experimenting with another getter
logMessageMessage:: LogMessage -> Maybe String 
logMessageMessage (LogMessage (Error _ ) _ msg) = Just msg
logMessageMessage (LogMessage Info _ msg) = Just msg
logMessageMessage (LogMessage Warning _ msg) = Just msg
logMessageMessage _ = Nothing

-- sort the messages. filter out 50 or greater.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map (\lm -> case logMessageMessage(lm) of Just msg -> msg) . 
                inOrder . build $ takeWhile (\lm -> case errorTimeStamp(lm) of 
                                                         Nothing -> False
                                                         Just ts -> ts >= 50) lms
