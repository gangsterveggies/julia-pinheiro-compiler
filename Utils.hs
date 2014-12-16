module Utils where

fromJust :: String -> Maybe b -> b
fromJust msg Nothing = error ("ERROR: " ++ msg)
fromJust _ (Just x) = x

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

testAndError :: Bool -> String -> a -> Maybe a
testAndError True _ vl = Just vl
testAndError False msg _ = error msg

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

last3 :: (a, b, c) -> c
last3 (_, _, c) = c
