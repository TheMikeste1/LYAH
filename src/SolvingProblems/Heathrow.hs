module SolvingProblems.Heathrow where

import System.Directory

calculateMinimumPath :: (Num a, Ord a) => [[a]] -> a
calculateMinimumPath [] = 0
calculateMinimumPath (a:b:c:xs)
   | not $ null xs = error "Too many paths!"
   | length a /= length b || length a /= length c
      = error "Paths A and B and C must be equal length!"
   | otherwise = calculateCost "" a b c
   where
     calculateCost _ [] _ _ = 0
     calculateCost _ _ [] _ = 0
     calculateCost _ _ _ [] = 0
     calculateCost current (a:as) (b:bs) (c:cs) =
      case current of
         "" -> min a b
             + calculateCost (if a < b then "A" else "B") as bs (c:cs)
         "A" -> min a (b + c)
             + calculateCost (if a < b + c then "A" else "B") as bs cs
         "B" -> min b (a + c)
             + calculateCost (if a + c < b then "A" else "B") as bs cs


parseFile :: FilePath -> IO [[Int]]
parseFile filename = do
  fileExists <- doesFileExist filename
  if fileExists
   then do
     contents <- readFile filename
     -- Turn contents into numbers and assign numbers for separation.
     let zippedValues = map read $ lines contents
     -- Divvy the values into 3 lists
     return [
         getEveryThird $ 0:0:zippedValues,  -- Add 0 as buffer so it starts
         getEveryThird $ 0:zippedValues,    --  where we want.
         getEveryThird zippedValues
      ]
  else return []
  where getEveryThird xs = case drop 2 xs of
            []     -> []
            (y:ys) -> y:getEveryThird ys
