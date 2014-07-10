--{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT 

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) =  (eval e1) + (eval e2)
eval (Mul e1 e2) =  (eval e1) * (eval e2)
