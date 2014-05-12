module Console where

import Game

-- loop that actually plays the game.
play :: Game -> String -> IO String
play g@(Game l) m = do
    putStrLn $ prompt m l
    x <- fmap read getLine
    case move g x of
        g'@(Game _, _) -> uncurry play g'
        (_, m')        -> return m'

-- Main entry: play the game and announce the results
main :: IO ()
main = do
       m <- play (Game 8) "Hello. "
       putStrLn m
