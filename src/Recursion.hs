module Recursion where


fibonacci :: (Ord a, Integral a) => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x
 | x < 0 = error "Fibonacci must be called on positive integers!"
 | otherwise = fibonacci (x - 1) + fibonacci (x - 2)


-- Note this isn't a "true" quicksort implementation, as it does not sort in place.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  smaller ++ [x] ++ larger
  where smaller = quicksort [ord | ord <- xs, ord <= x]
        larger  = quicksort [ord | ord <- xs, ord > x]
