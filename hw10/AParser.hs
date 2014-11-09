{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative
import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

--didn't use this.  not sure if I'm doing this wrong. 
-- is this mean to take a function  and a tuple of parsed output so far + a string and return more parsed output and a string. 
first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)

instance Functor Parser where
-- fmap :: (a -> b) -> f a -> f b 
-- fmap :: (a -> b) -> Parser a -> Parser b 
  fmap h (Parser g) = Parser fn 
    where 
      fn s = case g s of 
              Nothing -> Nothing
              Just (x, xs) -> Just (h x, xs)

-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
-- (<*>) :: Parser (a -> b ) -> Parser a -> Parser b
  (Parser g) <*> (Parser h) = Parser fn
    where fn s = case g s of 
                  Nothing -> Nothing
                  Just (fab, xs) -> case h xs of 
                                     Nothing      -> Nothing
                                     Just (a, ys) -> Just (fab a, ys) 

instance Alternative Parser where
  empty = Parser $ const Nothing -- creates a function that takes anything and returns nothing. 
  Parser p1 <|> Parser p2 = Parser fn 
    where fn s = case p1 s of 
                   Just(a, rest) -> Just(a, rest)
                   Nothing       -> p2 s

abParser :: Parser (Char, Char)
abParser = (\x y -> (x, y)) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\i j -> [i, j]) <$> posInt <*> posInt 
