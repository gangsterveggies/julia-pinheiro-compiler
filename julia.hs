module Main where
import Lexer
import Parser
import TCompile
import qualified Data.Map as HashTable

type HashTable = HashTable.Map String Int

fromJust :: Maybe b -> b
fromJust Nothing = error "ERROR: variable not defined"
fromJust (Just x) = x

mipsify :: HashTable -> Int -> [Code] -> String
mipsify _ _ [] = ""
mipsify hs maxN (x:xs) = mCode ++ (mipsify hs1 maxN1 xs)
  where (mCode, hs1, maxN1) = translate hs maxN x

translate :: HashTable -> Int -> Code -> (String, HashTable, Int)
translate hs maxN (OpAt, TVar x, p, Null) = (getCode1 ++ getCode2 ++ "sw $t1, ($t0)\n", hs1, maxN1)
  where getCode1 = getValue hs maxN "$t1" p
        (getCode2, hs1, maxN1) = setVariable hs maxN "$t0" x

setVariable :: HashTable -> Int -> String -> String -> (String, HashTable, Int)
setVariable hs maxN register var
  | varV == Nothing = ("addi $sp, $sp, -4\n" ++ "la " ++ register ++ ", 0($sp)\n", (HashTable.insert var maxN hs), maxN + 1)
  | otherwise = let varN = 4 * (maxN - 1 - fromJust(varV))
                in ("la " ++ register ++ ", " ++ (show varN) ++ "($sp)\n", hs, maxN)
  where varV = HashTable.lookup var hs

getVariable :: HashTable -> Int -> String -> Int
getVariable hs maxN var = 4 * (maxN - 1 - fromJust(HashTable.lookup var hs))

getValue :: HashTable -> Int -> String -> Value -> String
getValue _ _ register (Num a) = "li " ++ register ++ ", " ++ (show a) ++ "\n"
getValue hs maxN register (TVar a) = "lw " ++ register ++ ", " ++ (show var) ++ "($sp)\n"
  where var = getVariable hs maxN a

main = do
  inStr <- getContents
  let hs =  HashTable.empty
  let mipsCode = mipsify hs 0 (fst(compileCmd 0 (parse(alexScanTokensWrapper inStr))))
--  let parseTree = fst(compileCmd 0 (parse(alexScanTokensWrapper inStr)))
  writeFile "code.asm" (mipsCode)
  putStrLn "julia-pinheiro is done compiling!"
