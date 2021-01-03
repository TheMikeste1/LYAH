module Main where

import Modules as Mod

main :: IO ()
main = putStrLn (Mod.caesarCipher 3 "Heey")  --putStrLn "TEST"
