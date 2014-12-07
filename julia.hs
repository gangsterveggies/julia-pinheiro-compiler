module Main where
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
translate sc (OpPrint, x, Null, Null)
  | tp == TInt = (getCode ++ "\tli $v0, 1\n\tmove $a0, $t0\n\tsyscall\n\n\tla $a0, lc\n\tli $v0, 4\n\tsyscall\n\n", sc)
  | tp == TFloat = (getCode ++ "\tli $v0, 2\n\tmov.s $f12, $f0\n\tsyscall\n\n\tla $a0, lc\n\tli $v0, 4\n\tsyscall\n\n", sc)
  | otherwise = ("", sc)
  where getCode = getValue sc 0 x
        tp = getValueType x
translate sc (op, TVar (x, tp), p1, p2) = (getCode1 ++ getCode2 ++ getCode3 ++ "\n\t" ++ (translateOperation op tp) ++" $" ++ (getRegisterType tp) ++ "3, $" ++ (getRegisterType tp) ++ "1, $" ++ (getRegisterType tp) ++ "2\n\t" ++ (getStoreType tp) ++ " $" ++ (getRegisterType tp) ++ "3, ($t0)\n\n", sc1)
  where getCode1 = getValue sc 1 p1
        getCode2 = getValue sc 2 p2
        (getCode3, sc1) = setVariable sc 0 x tp

translateOperation :: Op -> Type -> String
translateOperation OpAdd TInt = "add"
translateOperation OpSub TInt = "sub"
translateOperation OpMul TInt = "mul"
translateOperation OpDiv TInt = "div"
translateOperation OpMod TInt = "rem"
translateOperation OpAdd TFloat = "add.s"
translateOperation OpSub TFloat = "sub.s"
translateOperation OpMul TFloat = "mul.s"
translateOperation OpDiv TFloat = "div.s"
translateOperation _ _ = error "ERROR: invalid operation"

getValueType :: Value -> Type
getValueType (Const a) = convertType a
getValueType (TVar (_, tp)) = tp

getRegisterType :: Type -> String
getRegisterType TInt = "t"
getRegisterType TFloat = "f"

getStoreType :: Type -> String
getStoreType TInt = "sw"
getStoreType TFloat = "swc1"

getLoadType :: Type -> String
getLoadType TInt = "lw"
getLoadType TFloat = "lwc1"

main = do
  inStr <- getContents
  let hs = Scope.empty
  let tokenList = alexScanTokensWrapper inStr
  let parseTree = parse tokenList
  let taddCode = compile parseTree
  let staticCode = staticAnalysis taddCode
  let mipsCode = "\t.text\n" ++ (mipsify hs staticCode)

--  putStrLn (show inStr)
--  putStrLn (show tokenList)
--  putStrLn (show parseTree)
--  putStrLn (show taddCode)
--  putStrLn (show staticCode)
  writeFile "code.asm" (mipsCode)
  putStrLn "julia-pinheiro is done compiling!"
