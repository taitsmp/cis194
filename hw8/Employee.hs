module Employee where

import           Data.Tree
import           Data.Monoid

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun  = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Show, Read, Eq)

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

testCompany2 :: Tree Employee
testCompany2
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3) -- (8, 8)  -- should this be (8,9) ? John 1 + Sue 5 + Fred 3 = 9
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

-- A type to store a list of guests and their total fun score.
data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

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
-- this is the dumb way.  If the boss is not invited always invite the sub-boss
nextLevel b sl = let a = mconcat sl 
-- this way of calculating second guestlist seems smarter.  not yet combined into result.
                     c = foldr (\(e,f) acc -> acc `mappend` moreFun e f  ) (GL [] 0) sl
                 in (glCons b $ fst a, snd a)




