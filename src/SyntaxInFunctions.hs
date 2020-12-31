module SyntaxInFunctions where

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z


capital :: String -> String
capital "" = "Empty string, whoops!"
capital xxs@(x:xs) = "The first letter of " ++ xxs ++ " is " ++ [x]


bmiTell :: (RealFloat a, Show a) => a -> a -> String  
bmiTell weight height  
   | bmi <= underweight = "You're underweight! " ++ show bmi
   | bmi <= normal      = "You're normal! "      ++ show bmi
   | bmi <= overweight  = "You're overweight! "  ++ show bmi
   | otherwise          = "See a doctor. "       ++ show bmi
   where bmi = (weight / height) ** 2 
         underweight = 18.5  
         normal = 25.0  
         overweight = 30.0 
          
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
   let sideArea = 2 * pi * r * h  
       topArea = pi * r ** 2  
   in  sideArea + 2 * topArea  