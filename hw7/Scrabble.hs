{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.List
import Data.Char
import Data.Maybe

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+) 

score' :: Char -> Int
score' c = fromMaybe 0 $ findIndex (\a -> toLower c `elem` a) [[], "aeilnorst", "dg", "bcmp", "fhvwy", "k", [], [], "j", [], "qz"]

score :: Char -> Score
score c = Score (score' c)
           
scoreString :: String -> Score
scoreString s = mconcat $ map score s


