module Scope where
import Stack as Stack
import qualified Data.Map as HashTable

type HashTable = HashTable.Map String Int

type Scope = (Stack (HashTable, Int), Int)

empty :: Scope
empty = (Stack.empty, 0)

enscope :: Scope -> Scope
enscope (st, i) = (Stack.push (HashTable.empty, 0) st, i)

descope :: Scope -> Scope
descope (st, i) = (Stack.pop st, i - (snd (Stack.top st)))

insert :: String -> Scope -> Scope
insert var (st, i) = (Stack.changeTop ((HashTable.insert var i hs), hi + 1) st, i + 1)
  where (hs, hi) = Stack.top st

lookup :: String -> Scope -> (Maybe Int)
lookup var (st, _)
  | Stack.isEmpty st = Nothing
  | otherwise = let hs = fst (Stack.top st)
                in let ans = HashTable.lookup var hs
                   in if ans == Nothing then Scope.lookup var (Stack.pop st, 0)
                      else ans

size :: Scope -> Int
size (_, vl) = vl

topSize :: Scope -> Int
topSize (st, _) = snd (Stack.top st)

{- main = do
  let sc = Main.insert "b" (Main.insert "a" (Main.enscope Main.empty))
  putStrLn (show (Main.lookup "a" sc))
  putStrLn (show (Main.lookup "b" sc))
  putStrLn (show (Main.lookup "c" sc))
  putStrLn ""
  
  let sc1 = Main.insert "c" (Main.enscope sc)
  putStrLn (show (Main.lookup "a" sc1))
  putStrLn (show (Main.lookup "b" sc1))
  putStrLn (show (Main.lookup "c" sc1))
  putStrLn ""

  let sc2 = Main.descope sc1
  putStrLn (show (Main.lookup "a" sc2))
  putStrLn (show (Main.lookup "b" sc2))
  putStrLn (show (Main.lookup "c" sc2)) -}
