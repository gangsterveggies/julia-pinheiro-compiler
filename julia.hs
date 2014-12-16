module Main where
import System.Exit
import System.Environment
import Data.List
import Utils
import Lexer
import Parser
import TCompile
import StaticAnalysis
import Scope

setVariable :: Scope -> Int -> String -> Type-> (String, Scope)
setVariable sc register var tp
  | varV == Nothing = ("\taddi $sp, $sp, -4\n" ++ "\tla $t" ++ (show register) ++ ", 0($sp)\n", Scope.insert var sc)
  | otherwise = let varN = 4 * (maxN - 1 - (fromJust "variable not defined" varV))
                in ("\tla $t" ++ (show register)  ++ ", " ++ (show varN) ++ "($sp)\n", sc)
  where varV = Scope.lookup var sc
        maxN = Scope.size sc

getVariable :: Scope -> String -> Int
getVariable sc var = 4 * (maxN - 1 - (fromJust "variable not defined" (Scope.lookup var sc)))
  where maxN = Scope.size sc

getValue :: Scope -> Int -> Value -> String
getValue _ register (Const (VTInt a)) = "\tli $t" ++ (show register) ++ ", " ++ (show a) ++ "\n"
getValue _ register (Const (VTFloat a)) = "\tli.s $f" ++ (show register) ++ ", " ++ (show a) ++ "\n"
getValue _ register (Const (VTBool a)) = "\tli $t" ++ (show register) ++ ", " ++ (show (boolToInt a)) ++ "\n"
getValue sc register (TVar (a, tp)) = "\t" ++ (getLoadType tp) ++  " $" ++ (getRegisterType tp) ++ (show register) ++ ", " ++ (show var) ++ "($sp)\n"
  where var = getVariable sc a

mipsify :: Scope -> [Code] -> String
mipsify _ [] = "\t.data\nlc:\t.asciiz \"\\n\"\n"
mipsify sc (x:xs) = mCode ++ (mipsify sc1 xs)
  where (mCode, sc1) = translate sc x

translate :: Scope -> Code -> (String, Scope)
translate sc (OpSc, Null, Null, Null) = ("", Scope.enscope sc True)
translate sc (OpSc, UVar _, Null, Null) = ("", Scope.enscope sc False)
translate sc (OpDeSc True, Null, Null, Null) = ("\taddi $sp, $sp, " ++ (show (4 * curN)) ++ "\n\n", Scope.descope sc)
  where curN = Scope.topSize sc
translate sc (OpDeSc False, Null, Null, Null) = ("\taddi $sp, $sp, " ++ (show (4 * curN)) ++ "\n\n", sc)
  where curN = Scope.topSize sc
translate sc (OpAt, TVar (x, tp), p, Null) = (getCode1 ++ getCode2 ++ "\t" ++ (getStoreType tp) ++ " $" ++ (getRegisterType tp) ++ "1, ($t0)\n\n", sc1)
  where getCode1 = getValue sc 1 p
        (getCode2, sc1) = setVariable sc 0 x tp
translate sc (OpLb, Label s, Null, Null) = ("L" ++ s ++ ":\n", sc)
translate sc (OpJp, Label s, Null, Null) = ("\tj L" ++ s ++ "\n", sc)
translate sc (OpFunc, TVar (s, _), Null, Null) = (s ++ ":\n" ++ getCode ++ "\tsw $ra, ($t0)\n\n", sc1)
  where (getCode, sc1) = setVariable sc 0 "0r" TInt
translate sc (OpParam vl False, TVar (var, tp), Null, Null) = (getCode ++ "\tlw $a" ++ (show vl) ++ ", ($t0)\n\n", sc1)
  where (getCode, sc1) = setVariable sc 0 var tp
translate sc (OpParam vl True, TVar (var, tp), Null, Null) = (getCode ++ "\tsw $a" ++ (show vl) ++ ", ($t0)\n\n", sc1)
  where (getCode, sc1) = setVariable sc 0 var tp
translate sc (OpCall, TVar (var, tp), UVar f, Null) = ("\tjal " ++ f ++ "\n\n" ++ getCode ++ "\tsw $v0, ($t0)\n\n", sc1)
  where (getCode, sc1) = setVariable sc 0 var tp
translate sc (OpRd, TVar (var, tp), Null, Null) = ("\tli $v0, " ++ (getReadType tp) ++ "\n\tsyscall\n\n" ++ getCode ++ "\t" ++ (getStoreType tp) ++ " $" ++ (getReturnType tp) ++ "0, ($t0)\n\n", sc1)
  where (getCode, sc1) = setVariable sc 0 var tp
        
translate sc (OpJr, Null, Null, Null) = ("\tjr $ra\n\n", sc)
translate sc (OpRet, TVar (var, tp), Null, Null) = (getCode1 ++ "\tlw $v0, ($t0)\n" ++ getCode2 ++ "\tlw $ra, ($t1)\n\n", sc2)
  where (getCode1, sc1) = setVariable sc 0 var tp
        (getCode2, sc2) = setVariable sc1 1 "0r" TInt
translate sc (OpEnd, Null, Null, Null) = ("\tli $v0, 10\n\tsyscall\n\n", sc)
translate sc (OpIfFalse, x, Label s, Null) = (getCode ++ "\tbeqz $t0, L" ++ s ++ "\n\n", sc)
  where getCode = getValue sc 0 x
translate sc (OpPrintLC, Null, Null, Null) = ("\tla $a0, lc\n\tli $v0, 4\n\tsyscall\n\n", sc)
translate sc (OpPrint, x, Null, Null)
  | tp == TInt = (getCode ++ "\tli $v0, 1\n\tmove $a0, $t0\n\tsyscall\n\n", sc)
  | tp == TBool = (getCode ++ "\tli $v0, 1\n\tmove $a0, $t0\n\tsyscall\n\n", sc)
  | tp == TFloat = (getCode ++ "\tli $v0, 2\n\tmov.s $f12, $f0\n\tsyscall\n\n", sc)
  | otherwise = ("", sc)
  where getCode = getValue sc 0 x
        tp = getValueType x
translate sc (OpOp op, TVar (x, tp), p1, p2) = (getCode1 ++ getCode2 ++ getCode3 ++ "\n\t" ++ (translateOperation op tp tp1 tp2) ++" $" ++ (getRegisterType tp) ++ "3, $" ++ (getRegisterType tp) ++ "1, $" ++ (getRegisterType tp) ++ "2\n\t" ++ (getStoreType tp) ++ " $" ++ (getRegisterType tp) ++ "3, ($t0)\n\n", sc1)
  where getCode1 = getValue sc 1 p1
        getCode2 = getValue sc 2 p2
        (getCode3, sc1) = setVariable sc 0 x tp
        tp1 = getValueType p1
        tp2 = getValueType p2

translateOperation :: String -> Type -> Type -> Type -> String
translateOperation "+" TInt _ _ = "add"
translateOperation "-" TInt _ _ = "sub"
translateOperation "*" TInt _ _ = "mul"
translateOperation "/" TInt _ _ = "div"
translateOperation "%" TInt _ _ = "rem"
translateOperation "+" TFloat _ _ = "add.s"
translateOperation "-" TFloat _ _ = "sub.s"
translateOperation "*" TFloat _ _ = "mul.s"
translateOperation "/" TFloat _ _ = "div.s"

translateOperation "==" TBool TInt _ = "seq"
translateOperation "!=" TBool TInt _ = "sne"
translateOperation "<" TBool TInt _ = "slt"
translateOperation ">" TBool TInt _ = "sgt"
translateOperation "<=" TBool TInt _ = "sle"
translateOperation ">=" TBool TInt _ = "sge"

{-translateOperation OpEq TBool TFloat _ = "c.eq.s 0 $f1 $f2\n\tmovt $t3, $zero"
translateOperation OpNEq TBool TFloat _ = "rem"
translateOperation OpLs TBool TFloat _ = "rem"
translateOperation OpGt TBool TFloat _ = "rem"
translateOperation OpLq TBool TFloat _ = "rem"
translateOperation OpGq TBool TFloat _ = "rem"-}
translateOperation _ _ _ _ = error "ERROR: invalid operation"

getValueType :: Value -> Type
getValueType (Const a) = convertType a
getValueType (TVar (_, tp)) = tp

getRegisterType :: Type -> String
getRegisterType TInt = "t"
getRegisterType TFloat = "f"
getRegisterType TBool = "t"

getStoreType :: Type -> String
getStoreType TInt = "sw"
getStoreType TFloat = "swc1"
getStoreType TBool = "sw"

getLoadType :: Type -> String
getLoadType TInt = "lw"
getLoadType TFloat = "lwc1"
getLoadType TBool = "lw"

getReturnType :: Type -> String
getReturnType TInt = "v"
getReturnType TFloat = "f"
getReturnType TBool = "v"

getReadType :: Type -> String
getReadType TInt = "5"
getReadType TFloat = "6"
getReadType TBool = "5"

data Flag = Help | Output String | PrintTAdd | PrintParse | Quiet | File String deriving (Show, Eq, Ord)

argumentHandle :: [String] -> [Flag]
argumentHandle [] = []
argumentHandle ("-h":xs) = Help : (argumentHandle xs)
argumentHandle ("-q":xs) = Quiet : (argumentHandle xs)
argumentHandle ("--print-parse":xs) = PrintParse : (argumentHandle xs)
argumentHandle ("--print-tadd":xs) = PrintTAdd : (argumentHandle xs)
argumentHandle ("-o":f:xs) = (Output f) : (argumentHandle xs)
argumentHandle (f:xs) = (File f) : (argumentHandle xs)

getOutputFile :: [Flag] -> String
getOutputFile [] = "a.asm"
getOutputFile (Output s:xs) = s ++ ".asm"
getOutputFile (_:xs) = getOutputFile xs

getInputFile :: [Flag] -> Maybe String
getInputFile [] = Nothing
getInputFile (File s:xs) = Just s
getInputFile (_:xs) = getInputFile xs

checkFlag :: Flag -> [Flag] -> Bool
checkFlag _ [] = False
checkFlag f (p:xs) = if f == p then True else checkFlag f xs

helpText :: String
helpText = "Usage:\n\n\tjpc [command-line-options] <input-file>\n\nAvailable options:\n\n\t-h\t\tDisplays this amazing help file\n\t-o <file>\tSet '<file>.asm' as output file (default 'a.asm')\n\t--print-parse\tPrint internal parse code in haskell's structures\n\t--print-tadd\tPrint internal three address code in haskell's structures\n\t-q\t\tquiet compile (don't print that huge header nor the final message)\n"

logoText :: String
logoText = "   _       _ _(_)_     |  A fresh haskell julia compiler to mips\r\n  (_)     | (_) (_)    |  Documentation: run <jpc -h> for help\r\n   _ _   _| |_  __ _   |\r\n  | | | | | | |/ _` |  |  Version 0.3 (2014-12-16 23:59 UTC)\r\n  | | |_| | | | (_| |  |  Official: http://julialang.org\r\n _/ |\\__'_|_|_|\\__'_|  |\r\n|__/                   |  from julia to mips in less than a second!\r\n                   _       _          _\r\n                  (_)     | |        (_)\r\n             _ __  _ _ __ | |__   ___ _ _ __ ___\r\n            | '_ \\| | '_ \\| '_ \\ / _ \\ | '__/ _ \\\r\n    ______  | |_) | | | | | | | |  __/ | | | (_) |\r\n   |______| | .__/|_|_| |_|_| |_|\\___|_|_|  \\___/\r\n            | |\r\n            |_|\n\n\tBy Pedro Paredes and Filipe Figueiredo (DCC/FCUP)\n\n"

main :: IO ()
main = do
  argList <- getArgs
  let flagList = sort (argumentHandle(fmap id argList))
  if (checkFlag Help flagList) then do
    putStr logoText
    putStr helpText
  else do
    let quietCompile = checkFlag Quiet flagList

    if (not quietCompile) then putStr logoText
    else return ()

    let hs = Scope.empty
    let outputFile = getOutputFile flagList
    rawCode <- readFile (fromJust "no input files\nTry the '-h' option for basic information" (getInputFile flagList))

    let tokenList = alexScanTokensWrapper rawCode
    let parseTree = parse tokenList
    let taddCode = compile parseTree
    let staticCode = staticAnalysis taddCode
    let mipsCode = "\t.text\n" ++ (mipsify hs staticCode)

    let printParse = checkFlag PrintParse flagList
    let printTAdd = checkFlag PrintTAdd flagList
--  putStrLn ((show rawCode) ++ "\n")
--  putStrLn ((show tokenList) ++ "\n")
--  putStrLn ((show parseTree) ++ "\n")
--  putStrLn ((show taddCode) ++ "\n")
--  putStrLn ((show staticCode) ++ "\n")
    writeFile outputFile (mipsCode)
    
    if printParse then putStrLn ("Parse Tree:\n" ++ (show parseTree) ++ "\n")
    else return ()

    if printTAdd then putStrLn ("Three Address Code:\n" ++ (show staticCode) ++ "\n")
    else return ()

    if (not quietCompile) then putStrLn "julia-pinheiro is done compiling!"
    else return ()
{-

Final things to do:
  Fix li float
  Add arrays
  Add strings
  Add code import
  Print strings

-}
