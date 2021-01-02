module HigherOrderFunctions where

collatzChain :: (Integral a, Show a) => a -> [a]
collatzChain 1 = [1]
collatzChain x
   | x <= 0  = error $ "Invalid chain value: \"" ++ show x ++ "\". Values must be greater than 0!"
   | even x = x:collatzChain (div x 2)
   | odd x  = x:collatzChain (x * 3 + 1)
   | otherwise = error $ "Invalid chain value: \"" ++ show x ++ "\""
 