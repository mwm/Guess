module Game where


data Game = Won | Lost | Game Int

invalidMove:: Int -> Bool
invalidMove m = m /= 1 && m /= 2

move :: Game -> Int -> (String, Game)
move (Game l) m | invalidMove m       = ("You may only take 1 or 2 matches at a time.", Game l)
                | m >= l              = ("Congratulations, you won.", Won)
                | l - m <= 2          = ("I take " ++ show (l - m) ++ " and you lose.", Lost)
                | rem (l - m) 3 /= 0  = let m' = rem (l - m) 3 in ("I take " ++ show m' ++ ".", Game (l - m - m'))
                | otherwise           = ("I take 1.", Game (l - m - 1))