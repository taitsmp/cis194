-- CIS 194 Homework 2

module Log where

import Control.Applicative
import Data.List

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- extract ints from "words" in a message
messageInts :: [String] -> [Int]
messageInts wds = foldl (\acc x -> let i = reads x :: [(Int, String)] in if null i then acc else ((fst(head i)) ::Int):acc) [] wds

parseMessage :: String -> LogMessage
parseMessage ('I':' ':xs) = (LogMessage Info  p1 txt)
   where wds = words xs
         p1  = read (head wds) :: Int
         txt = intercalate " " wds 
parseMessage ('E':' ':xs) = (LogMessage (Error p1) (read (wds !! 1) :: Int) (intercalate " " $ tail wds))
   where wds = words xs
         p1  = read (head wds) :: Int
-- parseMessage (stripPrefix "I " -> Just restOfString) =

parseMessage1 :: String -> LogMessage
parseMessage1 msg = case msg of ('I':' ':xs) -> (LogMessage Info  123 xs)

-- parseMessage2 :: String -> LogMessage
-- parseMessage2 msg = pm msg
--    where             wds = words msg
--          pm ('I':' ':xs) = (LogMessage Info  123 xs)
--         pm ('E':' ':xs) = (LogMessage Error  123 xs)


parseMessage3 :: String -> LogMessage
parseMessage3 (t:' ':xs)
              | t == 'I' =  (LogMessage Info p1 xs)
              | t == 'E' =  (LogMessage (Error p1) (read (wds !! 1) :: Int) (intercalate " " $ tail wds))
              where wds = words xs
                    p1  = read (head wds) :: Int
                    txt = intercalate " " wds 


-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file
