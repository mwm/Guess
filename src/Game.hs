module Game (Game (..), move, prompt) where

data Game = Illegal | Won | Lost | Game Int deriving (Show)
type Move = Int

-- Create a prompt for the current game and message.
prompt :: String -> Int -> String
prompt m l = m ++ "There are " ++ show l ++ " matches. How many do you take? "

-- Given a game and a move, provide a description for the move.
describeMove :: Game -> Move -> String
describeMove g m =
    case g of
        Won      -> "You won. "
        Lost     -> "I took " ++ show m ++ " and you lost. "
        Illegal  -> "You can only take 1 or 2 matches. Taking the last match wins the game. "
        (Game _) -> "I took " ++ show m ++ ". "

-- Given a game and a move, return the Game resulting from the Move.
makeMove :: Game -> Move -> Game
makeMove g@(Game l) m | m /= 1 && m /= 2 = Illegal
                      | m >= l           = Won
                      | otherwise        = Game $ l - m

-- Given a Game, find the best move for it
findMove :: Game -> Move
findMove (Game l) | l <= 2       = l
                  | rem l 3 /= 0 = rem l 3
                  | otherwise    = 1

-- Given a game and player move, calculate computer move and
-- return (message, new game)
move :: Game -> Move -> (Game, String)
move g m = case makeMove g m of
               Illegal -> (g, describeMove Illegal 1)
               Won     -> (Won, describeMove Won 1)
               Lost    -> (undefined, "Can't happen! ")
               g'      -> let m' = findMove g' in
                              case makeMove g' m' of
                                  Won  -> (Lost, describeMove Lost m')
                                  Lost -> (undefined, "Can't happen! ")
                                  g''  -> (g'', describeMove g'' m')