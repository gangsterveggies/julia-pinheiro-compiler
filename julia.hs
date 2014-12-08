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
translate sc (OpSc, Null, Null, Null) = ("", Scope.enscope sc)
translate sc (OpDeSc, Null, Null, Null) = ("\taddi $sp, $sp, " ++ (show (4 * curN)) ++ "\n\n", Scope.descope sc)
  where curN = Scope.topSize sc
translate sc (OpAt, TVar (x, tp), p, Null) = (getCode1 ++ getCode2 ++ "\t" ++ (getStoreType tp) ++ " $" ++ (getRegisterType tp) ++ "1, ($t0)\n\n", sc1)
  where getCode1 = getValue sc 1 p
        (getCode2, sc1) = setVariable sc 0 x tp
translate sc (OpLb, Label s, Null, Null) = ("L" ++ s ++ ":\n", sc)
translate sc (OpJp, Label s, Null, Null) = ("\tj L" ++ s ++ "\n", sc)
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
translate sc (op, TVar (x, tp), p1, p2) = (getCode1 ++ getCode2 ++ getCode3 ++ "\n\t" ++ (translateOperation op tp tp1 tp2) ++" $" ++ (getRegisterType tp) ++ "3, $" ++ (getRegisterType tp) ++ "1, $" ++ (getRegisterType tp) ++ "2\n\t" ++ (getStoreType tp) ++ " $" ++ (getRegisterType tp) ++ "3, ($t0)\n\n", sc1)
  where getCode1 = getValue sc 1 p1
        getCode2 = getValue sc 2 p2
        (getCode3, sc1) = setVariable sc 0 x tp
        tp1 = getValueType p1
        tp2 = getValueType p2

translateOperation :: Op -> Type -> Type -> Type -> String
translateOperation OpAdd TInt _ _ = "add"
translateOperation OpSub TInt _ _ = "sub"
translateOperation OpMul TInt _ _ = "mul"
translateOperation OpDiv TInt _ _ = "div"
translateOperation OpMod TInt _ _ = "rem"
translateOperation OpAdd TFloat _ _ = "add.s"
translateOperation OpSub TFloat _ _ = "sub.s"
translateOperation OpMul TFloat _ _ = "mul.s"
translateOperation OpDiv TFloat _ _ = "div.s"

translateOperation OpEq TBool TInt _ = "seq"
translateOperation OpNEq TBool TInt _ = "sne"
translateOperation OpLs TBool TInt _ = "slt"
translateOperation OpGt TBool TInt _ = "sgt"
translateOperation OpLq TBool TInt _ = "sle"
translateOperation OpGq TBool TInt _ = "sge"

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

argumentHandle [] = getContents
argumentHandle fs = concat `fmap` mapM readFile fs

main = do
  rawCode <- getArgs >>= argumentHandle
  let hs = Scope.empty
  let tokenList = alexScanTokensWrapper rawCode
  let parseTree = parse tokenList
  let taddCode = compile parseTree
  let staticCode = staticAnalysis taddCode
--  let staticCode = [(OpSc,Null,Null,Null),(OpAt,TVar ("a",TInt),Const (VTInt 0),Null),(OpAt,TVar ("b",TInt),Const (VTInt 1),Null), (OpAt,TVar ("c",TInt),Const (VTInt 10),Null), (OpLb,Label "0",Null,Null),(OpGt,TVar ("1t",TBool),TVar ("c",TInt),Const (VTInt 0)), (OpIfFalse,TVar ("1t",TBool),Label "1",Null), (OpSc,Null,Null,Null), (OpAt,TVar ("tmp",TInt),TVar ("b",TInt),Null), (OpAdd,TVar ("b",TInt),TVar ("a",TInt),TVar ("b",TInt)), (OpAt,TVar ("a",TInt),TVar ("tmp",TInt),Null), (OpSub,TVar ("c",TInt),TVar ("c",TInt),Const (VTInt 1)), (OpPrint,TVar ("a",TInt),Null,Null), (OpDeSc,Null,Null,Null), (OpJp,Label "0",Null,Null), (OpLb,Label "1",Null,Null), (OpDeSc,Null,Null,Null)]
  let mipsCode = "\t.text\n" ++ (mipsify hs staticCode)

--  putStrLn (show inStr)
--  putStrLn (show tokenList)
--  putStrLn (show parseTree)
--  putStrLn (show taddCode)
--  putStrLn (show staticCode)
  writeFile "code.asm" (mipsCode)
  putStrLn "julia-pinheiro is done compiling!"
