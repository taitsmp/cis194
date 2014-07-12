{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ExprT where

import Prelude

class Expr a where
   lit :: Integer -> a
   add :: a -> a -> a
   mul :: a -> a -> a

data ExprT = Lit Integer
           | Add ExprT ExprT | Mul ExprT ExprT
  deriving (Show, Eq)

instance Expr ExprT where 
  lit i = Lit i
  mul i j = Mul i j
  add i j = Add i j 

instance Expr Integer where
  lit i = i
  mul i j = i * j
  add i j = i + j

instance Expr Bool where
  lit i = i > 0 
  mul i j = i &&  j
  add i j = i || j

-- needed all of the 'deriving' types to get this to work. Could not just do 'deriving(Integral)'
newtype Mod7 = Mod7 Integer deriving (Eq, Show, Enum, Ord, Real, Integral, Num)
newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax 
  mul = min
  add = max
 
-- next steps
-- 1. look up newtype on github. what do you see?
-- 2. change newtype to data. Does it work? 

-- not working. type errors 
instance Expr Mod7 where
  lit  = Mod7
  add i j = (i + j) `mod` 7 
  mul i j = (i * j) `mod` 7
