module MakingOurOwnTypesAndTypeclasses.Typeclasses where

class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x MakingOurOwnTypesAndTypeclasses.Typeclasses./= y)  
    x /= y = not (x MakingOurOwnTypesAndTypeclasses.Typeclasses.== y) 
  
data TrafficLight = Red | Yellow | Green

instance MakingOurOwnTypesAndTypeclasses.Typeclasses.Eq TrafficLight where
    Red == Red       = True
    Green == Green   = True
    Yellow == Yellow = True
    _ == _           = False
    
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"

