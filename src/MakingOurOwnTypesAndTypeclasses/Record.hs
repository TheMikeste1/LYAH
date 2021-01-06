module MakingOurOwnTypesAndTypeclasses.Record where
  
data Person 
   = Person { firstName   :: String  
            , lastName    :: String  
            , age         :: Int  
            , height      :: Float  
            , phoneNumber :: String  
            , flavor      :: String  
            } deriving (Show)  

data Car = Car { company :: String  
               , model   :: String  
               , year    :: Int  
               } deriving (Show)
               
tellCar :: Car -> String  
tellCar Car {company = c, model = m, year = y} = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  



data Vector3 a = Vector3 a a a deriving (Show)
  
vplus3 :: (Num t) => Vector3 t -> Vector3 t -> Vector3 t
(Vector3 x1 y1 z1) `vplus3` (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
  
vectMult3 :: (Num t) => Vector3 t -> t -> Vector3 t
(Vector3 x1 y1 z1) `vectMult3` s = Vector3 (x1 * s) (y1 * s) (z1 * s)
  
dotProduct3 :: (Num t) => Vector3 t -> Vector3 t -> t
(Vector3 x1 y1 z1) `dotProduct3` (Vector3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

