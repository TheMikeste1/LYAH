module MakingOurOwnTypesAndTypeclasses.TypeSynonyms where

import qualified Data.Map as Map  

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (name, number) `elem` book

-----------------------------------------------------------------------------
data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
type LockerMap = Map.Map Int (LockerState, Code)  

{-
http://learnyouahaskell.com/making-our-own-types-and-typeclasses:
   "We could have used a 'Maybe a' to represent the result but then we wouldn't 
   know why we couldn't get the code. But now, we have information about the 
   failure in our result type."
-}
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing            -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> 
            if state == Free 
            then Right code  -- Return the code for the locker if it's available 
            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

