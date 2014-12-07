module Main where
import Lexer
import Parser
import TCompile
import Scope

fromJust :: Maybe b -> b
fromJust Nothing = error "ERROR: variable not defined"
fromJust (Just x) = x

setVariable :: Scope -> String -> String -> (String, Scope)
setVariable sc register var
  | varV == Nothing = ("\taddi $sp, $sp, -4\n" ++ "\tla " ++ register ++ ", 0($sp)\n", Scope.insert var sc)
  | otherwise = let varN = 4 * (maxN - 1 - fromJust(varV))
                in ("\tla " ++ register ++ ", " ++ (show varN) ++ "($sp)\n", sc)
  where varV = Scope.lookup var sc
        maxN = Scope.size sc

getVariable :: Scope -> String -> Int
getVariable sc var = 4 * (maxN - 1 - fromJust(Scope.lookup var sc))
  where maxN = Scope.size sc

getValue :: Scope -> String -> Value -> String
getValue _ register (Num a) = "\tli " ++ register ++ ", " ++ (show a) ++ "\n"
getValue sc register (TVar a) = "\tlw " ++ register ++ ", " ++ (show var) ++ "($sp)\n"
  where var = getVariable sc a

mipsify :: Scope -> [Code] -> String
mipsify _ [] = "\t.data\nlc:\t.asciiz \"\\n\"\n"
mipsify sc (x:xs) = mCode ++ (mipsify sc1 xs)
  where (mCode, sc1) = translate sc x

translate :: Scope -> Code -> (String, Scope)
translate sc (OpSc, Null, Null, Null) = ("", Scope.enscope sc)
translate sc (OpDeSc, Null, Null, Null) = ("\taddi $sp, $sp, " ++ (show (4 * curN)) ++ "\n\n", Scope.descope sc)
  where curN = Scope.topSize sc
translate sc (OpAt, TVar x, p, Null) = (getCode1 ++ getCode2 ++ "\tsw $t1, ($t0)\n\n", sc1)
  where getCode1 = getValue sc "$t1" p
        (getCode2, sc1) = setVariable sc "$t0" x
translate sc (OpLb, Label s, Null, Null) = ("L" ++ s ++ ":\n", sc)
translate sc (OpJp, Label s, Null, Null) = ("\tj L" ++ s ++ "\n", sc)
translate sc (OpIfFalse, x, Label s, Null) = (getCode ++ "\tbeqz $t0, L" ++ s ++ "\n\n", sc)
  where getCode = getValue sc "$t0" x
translate sc (OpPrint, x, Null, Null) = (getCode ++ "\tli $v0, 1\n\tmove $a0, $t0\n\tsyscall\n\n\tla $a0, lc\n\tli $v0, 4\n\tsyscall\n\n", sc)
  where getCode = getValue sc "$t0" x
translate sc (op, TVar x, p1, p2) = (getCode1 ++ getCode2 ++ getCode3 ++ "\n\t" ++ (translateOperation op) ++" $t3, $t1, $t2\n\tsw $t3, ($t0)\n\n", sc1)
  where getCode1 = getValue sc "$t1" p1
        getCode2 = getValue sc "$t2" p2
        (getCode3, sc1) = setVariable sc "$t0" x

translateOperation :: Op -> String
translateOperation OpAdd = "add"
translateOperation OpSub = "sub"
translateOperation OpMul = "mul"
translateOperation OpDiv = "div"
translateOperation OpMod = "rem"
translateOperation _ = error "ERROR: invalid intermediate code"

main = do
  inStr <- getContents
  let hs = Scope.empty
  let mipsCode = "\t.text\n" ++ (mipsify hs (compile(parse(alexScanTokensWrapper inStr))))
--  let taddCode = compile (parse(alexScanTokensWrapper inStr))
--  let parseTree = parse(alexScanTokensWrapper inStr)
--  let tokenList = alexScanTokensWrapper inStr
--  putStrLn (show inStr)
--  putStrLn (show tokenList)
--  putStrLn (show parseTree)
--  putStrLn (show taddCode)
  writeFile "code.asm" (mipsCode)
  putStrLn "julia-pinheiro is done compiling!"
