-- | Main entry point to the application.
module Console where

import Game


play :: Game -> IO String
play g@(Game l) = do
    putStrLn $ "There are " ++ show l ++ " matches. How many do you take? "
    m <- fmap read getLine
    case move g m of
        (m, g'@(Game _)) -> do {putStrLn m; play g'}
        (m, _) -> return m

-- | The main entry point.
main :: IO ()
main = play (Game 20) >>= putStrLn