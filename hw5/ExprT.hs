{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
module ExprT where

import Prelude
import qualified StackVM as SVM

class Expr a where
   lit :: Integer -> a
   add :: a -> a -> a
   mul :: a -> a -> a

data ExprT = Lit Integer
           | Add ExprT ExprT | Mul ExprT ExprT
  deriving (Show, Eq)

instance Expr ExprT where 
  lit = Lit
  mul = Mul 
  add = Add  

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
 
instance Expr Mod7 where
  lit  = Mod7
  add i j = (i + j) `mod` 7 
  mul i j = (i * j) `mod` 7

-- exercise 5
-- create an instance of the Expr type class for Program - expressions interpretted as compiled programs
-- like walking an abstract syntax tree.  trying to build up the program/stack. 
-- TODO: what to do with Bools? Am I missing something? Change def of Expr?
instance Expr SVM.Program where
  lit i = [SVM.PushI i] 
  mul i j = i ++ j ++ [SVM.Mul]
  add i j = i ++ j ++ [SVM.Add]

