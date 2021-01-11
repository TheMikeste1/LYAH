module SolvingProblems.RPN where

{-
Reverse Polish Notation calculator.
-}
rpn :: String -> Double
rpn = head . foldl evaluateRPNString [] . words
   where evaluateRPNString (b:a:xs) "+" = a + b : xs
         evaluateRPNString (b:a:xs) "-" = a - b : xs
         evaluateRPNString (b:a:xs) "*" = a * b : xs
         evaluateRPNString (b:a:xs) "/" = a / b : xs
         evaluateRPNString (b:a:xs) "^" = a ** b : xs
         evaluateRPNString (a:xs) "ln" = log a : xs
         evaluateRPNString xs "sum" = [sum xs]
         evaluateRPNString xs val = read val : xs
