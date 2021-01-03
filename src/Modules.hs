module Modules
(
   caesarCipher
) 
where

{-
List of standard modules:
   https://downloads.haskell.org/~ghc/latest/docs/html/libraries/
Brief descriptions of common functions and modules:
   http://learnyouahaskell.com/modules
-}

import Data.List (
   nub,  -- nub takes a list and returns the uniques
   partition  -- partition takes a predicate and a list and returns a list containing elements that follow the predicate and a list of those that do not.
   )
import Data.Char (
   chr,  -- Changes an int into the ASCii character associated with it.
   ord   -- Opposite of 'chr'; changes a character into its ASCii value.
   )

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


-- Note this isn't a "true" quicksort implementation, as it does not sort in place.
-- This implementation uses partition to avoid iterating over the list twice. Speed testing needs to be performed.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  quicksort smaller ++ x:quicksort larger
  where (smaller, larger) = partition (<=x) xs

-- This Caesar Cipher is restricted to the printable ASCii characters, specifically characters 32 to 126.
-- To remove this restriction, simply change the lambda to (+shift)
caesarCipher :: Int -> [Char] -> [Char]
caesarCipher shift msg = map (chr . (\x -> rem (x - 32 + shift) 95 + 32) . ord) msg

