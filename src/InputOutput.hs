module InputOutput where

import System.IO
import qualified Control.Monad

main :: IO ()
--main = do
--    putStrLn "Hello, what's your name?"
--    name <- getLine
--    putStrLn ("Hey " ++ name ++ ", you rock!")

--main = do
--    return ()
--    return "HAHAHA"
--    line <- getLine
--    return "BLAH BLAH BLAH"
--    return 4
--    putStrLn line

--main = do
--    c <- getChar
--    if c /= ' '
--     then do
--         putChar c
--         main
--     else return ()


--main = do
--    c <- getChar
--    Control.Monad.when (c /= ' ') $ do
--        putChar c
--        main

main = do
    colors <- Control.Monad.forM [1..4]
      (\a -> do
         putStr $ "Which color do you associate with the number " ++ show a ++ "? "
         hFlush stdout  -- This is needed so the console prints immediately, since putStr does not seem to work correctly.
         getLine)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    Control.Monad.mapM_ putStrLn colors


