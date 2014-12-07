module Utils where

fromJust :: String -> Maybe b -> b
fromJust msg Nothing = error ("ERROR: " ++ msg)
fromJust _ (Just x) = x
