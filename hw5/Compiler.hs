{-# LANGUAGE TypeSynonymInstances #-}
module Calculator where

import ExprT 
import StackVM

-- exercise 5
-- 
compile :: String -> Maybe Program
comile s = parseExp lit add mul s 
