module ExprT where

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

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax 
  mul = min
  add = max
 
-- not working. type errors 
instance Expr Mod7 where
  lit  = Mod7
  add i j = (i + j) `mod` 7  
  mul i j = (i * j) `mod` 7
