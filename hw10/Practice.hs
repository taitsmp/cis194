
module Practice where

type Name = String

data Employee = Employee { name  :: Name,
                           phone :: String } deriving Show
 

toEmployee :: (Name -> String -> Employee) -> (Maybe Name -> Maybe String -> Maybe Employee)
toEmployee bef = fn 
   where 
    fn (Just nm) (Just ph) = Just (bef nm ph)
    fn _ _ = Nothing
