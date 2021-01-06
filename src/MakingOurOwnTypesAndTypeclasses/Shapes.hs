module MakingOurOwnTypesAndTypeclasses.Shapes where


data Point  = Point Float Float deriving (Show)

data Shape
{-
Circle = center point, radius
-}
   = Circle Point Float
{-
Rectangle = top-left, bottom-right
-}
   | Rectangle Point Point
   deriving (Show)
-- data Shape

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = 
  Rectangle topLeft bottomRight
  where w = abs width / 2
        h = abs height / 2
        topLeft = Point (-w) h
        bottomRight = Point w (-h)
  
baseSquare :: Float -> Shape
baseSquare l = baseRectangle l l 
  
   

surface :: Shape -> Float
surface (Circle _ r) = pi * r ** 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)


distance2 :: Point -> Point -> Float
distance2 (Point x1 y1) (Point x2 y2) = sqrt $ ((x2 - x1) ** 2) + ((y2 - y1) ** 2)


nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy = Circle (Point (x + dx) (y + dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))



