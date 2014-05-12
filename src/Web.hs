module Web where

import MFlow.Wai.Blaze.Html.All

import Game

-- loop that actually plays the game.
play :: Game -> String -> View Html IO String
play g@(Game l) m =
    (toHtml (prompt m l) ++> br ++> getInt Nothing <! [("autofocus", "1")])
    `wcallback` \x ->
        case move g x of
            g'@(Game _, _) -> uncurry play g'
            (_, m') -> return m'

-- Main entry: play the game and announce the results
main :: IO ()
main = runNavigation "" . step $ do
    m <- page $ play (Game 20) "Hello. "
    page $ toHtml m ++> wlink () << " Another game?"
