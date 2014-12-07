module ScopeMap where
import Stack as Stack
import qualified Data.Map as HashTable

type HashTable a = HashTable.Map String a

type ScopeMap a = Stack (HashTable a)

empty :: ScopeMap a
empty = Stack.empty

enscope :: ScopeMap a -> ScopeMap a
enscope st = Stack.push HashTable.empty st

descope :: ScopeMap a -> ScopeMap a
descope st = Stack.pop st

insert :: String -> a -> ScopeMap a -> ScopeMap a
insert var vl st = Stack.changeTop (HashTable.insert var vl hs) st
  where hs = Stack.top st

lookup :: Eq a => String -> ScopeMap a -> (Maybe a)
lookup var st
  | Stack.isEmpty st = Nothing
  | otherwise = let hs = Stack.top st
                in let ans = HashTable.lookup var hs
                   in if ans == Nothing then ScopeMap.lookup var (Stack.pop st)
                      else ans

{- main = do
  let sc = ScopeMap.insert "b" 2 (ScopeMap.insert "a" 1 (ScopeMap.enscope ScopeMap.empty))
  putStrLn (show (ScopeMap.lookup "a" sc))
  putStrLn (show (ScopeMap.lookup "b" sc))
  putStrLn (show (ScopeMap.lookup "c" sc))
  putStrLn ""
  
  let sc1 = ScopeMap.insert "c" 3 (ScopeMap.enscope sc)
  putStrLn (show (ScopeMap.lookup "a" sc1))
  putStrLn (show (ScopeMap.lookup "b" sc1))
  putStrLn (show (ScopeMap.lookup "c" sc1))
  putStrLn ""

  let sc2 = ScopeMap.descope sc1
  putStrLn (show (ScopeMap.lookup "a" sc2))
  putStrLn (show (ScopeMap.lookup "b" sc2))
  putStrLn (show (ScopeMap.lookup "c" sc2))-}
