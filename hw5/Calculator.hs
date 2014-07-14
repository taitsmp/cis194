{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

import ExprT
import StackVM

-- exercise 5
-- create an instance of the Expr type class for Program - expressions interpretted as compiled programs
instance Expr Program where
  lit i = i > 0 
  mul i j = i &&  j
  add i j = i || j


