{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Tree
import Data.Monoid
import Employee				


glCons:: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (empFun e + f) 

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL el1 f1) (GL el2 f2) = GL (el1 ++ el2) (f1 + f2) 

moreFun:: GuestList -> GuestList -> GuestList
moreFun = max 

-- f must aggregate b values.
foldTree:: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node x ts) = f x (map (foldTree f) ts)  
-- needed help from internet to get this right -> http://web.cecs.pdx.edu/~mpj/pubs/springschool95.pdf

-- takes a boss and the tuples of the subdivision guest lists he manages (with and without him)
-- returns best list with boss and best list withou boss.
-- no tree operations involved for this. 
-- 1. invite the boss.  must choose the subtrees where subboss is not invited?
-- 2. don't invite the boss.  Can choose either subtrees where sub boss is invited or sub boss is not invited...
nextLevel:: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b sl = let wb = glCons b . mconcat $ map fst sl 
                     nb = foldr (\(e,f) acc -> acc `mappend` moreFun e f  ) (GL [] 0) sl
                 in (wb, nb)

-- accumulator will be a GuestList
-- use foldTree
maxFun :: Tree Employee -> GuestList
maxFun t = moreFun wb nb
           where (wb,nb) = foldTree nextLevel t 

-- print total fun
-- print guest list alphabetized by first name
main :: IO ()
main = do 
    putStrLn "Total Fun:"
    putStrLn $ "tait" ++ show fun
       where (GL _ fun) = maxFun testCompany 
