{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

import ExprT
import StackVM
import Parser

compile :: String -> Maybe StackVM.Program
compile s = parseExp lit add mul s
