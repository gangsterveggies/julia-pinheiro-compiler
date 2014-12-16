module Main where
import System.Environment
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
translate sc (OpParam vl False, TVar (var, tp), Null, Null) = (getCode ++ "\t" ++ (getLoadType tp) ++ " $a" ++ (show vl) ++ ", ($t0)\n\n", sc1)
  where (getCode, sc1) = setVariable sc 0 var tp
translate sc (OpParam vl True, TVar (var, tp), Null, Null) = (getCode ++ "\t" ++ (getStoreType tp) ++ " $a" ++ (show vl) ++ ", ($t0)\n\n", sc1)
  where (getCode, sc1) = setVariable sc 0 var tp
translate sc (OpCall, TVar (var, tp), UVar f, Null) = ("\tjal " ++ f ++ "\n\n" ++ getCode ++ "\t" ++ (getStoreType tp) ++ " $v0, ($t0)\n\n", sc1)
  where (getCode, sc1) = setVariable sc 0 var tp

translate sc (OpRd, TVar (var, tp), Null, Null) = ("\tli $v0, " ++ (getReadType tp) ++ "\n\tsyscall\n\n" ++ getCode ++ "\t" ++ (getStoreType tp) ++ " $" ++ (getReturnType tp) ++ "0, ($t0)\n\n", sc1)
  where (getCode, sc1) = setVariable sc 0 var tp
        
translate sc (OpJr, Null, Null, Null) = ("\tjr $ra\n\n", sc)
translate sc (OpRet, TVar (var, tp), Null, Null) = (getCode1 ++ "\t" ++ (getLoadType tp) ++ " $v0, ($t0)\n" ++ getCode2 ++ "\tlw $ra, ($t1)\n\n", sc2)
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

argumentHandle [] = getContents
argumentHandle fs = concat `fmap` mapM readFile fs

main = do
  rawCode <- getArgs >>= argumentHandle
  let hs = Scope.empty
  let tokenList = alexScanTokensWrapper rawCode
  let parseTree = parse tokenList
  let taddCode = compile parseTree
  let staticCode = staticAnalysis taddCode
  let mipsCode = "\t.text\n" ++ (mipsify hs staticCode)

--  putStrLn ((show rawCode) ++ "\n")
--  putStrLn ((show tokenList) ++ "\n")
--  putStrLn ((show parseTree) ++ "\n")
--  putStrLn ((show taddCode) ++ "\n")
--  putStrLn ((show staticCode) ++ "\n")
  writeFile "code.asm" (mipsCode)
  putStrLn "julia-pinheiro is done compiling!"
