{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

import ExprT
import StackVM

-- exercise 5
-- create an instance of the Expr type class for Program - expressions interpretted as compiled programs
-- like walking an abstract syntax tree.  trying to build up the program/stack. 
-- TODO: what to do with Bools? Am I missing something? Change def of Expr?
instance Expr Program where
  lit i = [PushI i] 
  mul i j = i ++ j ++ [StackVM.Mul]
  add i j = i ++ j ++ [StackVM.Add]


