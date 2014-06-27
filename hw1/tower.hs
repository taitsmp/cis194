
type Peg = String
type Move = (Peg, Peg)

-- hanoi should return a list of moves to be performed to move the stack of discs from the first peg to the second.
-- - move n-1 discs to from a to c using b as temp storage
-- - move top disc  from 'a' to b
-- - move n-1 discs from c to b using 'a' as temp storage 
-- p q r should be first last temp
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 p q _ = [(p,q)] --assume always start in same config. need to get to a place where you can move anything on first peg anywhere?
hanoi i p q r = (hanoi (i-1) p r q) ++ (hanoi 1 p q r) ++ (hanoi (i-1) r q p) 

