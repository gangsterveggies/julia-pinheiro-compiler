module TCompile where
import Utils
import Parser

import qualified Data.Map as HashTable

type HashTable a = HashTable.Map String a
type FuncTable = HashTable (Type, [Type])
data Op = OpOp String | OpFunc | OpParam Int Bool | OpCall | OpRet | OpEnd | OpJr
        | OpAt | OpJp | OpLb | OpIfFalse | OpPrint | OpPrintLC
        | OpSc | OpDeSc Bool deriving (Show)
data Value = Const ValueType | UVar String | Label String | TVar (String, Type) | Null deriving (Show)
type Code = (Op, Value, Value, Value)

compile :: [Func] -> [Code]
compile fs = (OpCall, TVar ("0t", TInt), UVar "main", Null) : (OpEnd, Null, Null, Null) : (compileFuncs hs fs)
  where hs = buildFuncMap fs

buildFuncMap :: [Func] -> FuncTable
buildFuncMap [] = HashTable.empty
buildFuncMap ((Func (nm, tp1) ls _):fs) = HashTable.insert nm (tp1, getArgTypes ls) (buildFuncMap fs)

getArgTypes :: [(String, Type)] -> [Type]
getArgTypes [] = []
getArgTypes ((_, tp):xs) = tp : (getArgTypes xs)

compileFuncs :: FuncTable -> [Func] -> [Code]
compileFuncs _ [] = []
compileFuncs hs (f:fs) = (compileFunc hs f) ++ (compileFuncs hs fs)

compileFunc :: FuncTable -> Func -> [Code]
compileFunc hs (Func nm args cmd) = [(OpSc, UVar "0j", Null, Null)]
                                    ++ [(OpFunc, TVar nm, Null, Null)]
                                    ++ paramsCode
                                    ++ code
                                    ++ [(OpDeSc True, Null, Null, Null)]
  where paramsCode = compileParamsDef args 0
        code = compileCmds hs (snd nm) cmd

compileParamsDef :: [(String, Type)] -> Int -> [Code]
compileParamsDef [] _ = []
compileParamsDef (x:xs) curPar = [(OpParam curPar True, TVar x, Null, Null)] ++ (compileParamsDef xs (curPar + 1))

compileParamsCall :: FuncTable -> [Expr] -> [Type] -> Int -> Int -> ([Code], Int)
compileParamsCall hs [] _ nx _ = ([], nx)
compileParamsCall hs (x:xs) (tp:ts) nx curPar = (expCode
                                              ++ [(OpParam curPar False, TVar (var, tp), Null, Null)]
                                              ++ paramsCode, nx2)
  where (var, expCode, nx1) = compileExp hs nx x
        (paramsCode, nx2) = (compileParamsCall hs xs ts nx1 (curPar + 1))

compileCmds :: FuncTable -> Type -> Command -> [Code]
compileCmds hs ftp cmd = (fst (compileCmd hs ftp 0 cmd))

compileCmd :: FuncTable -> Type -> Int -> Command -> ([Code], Int)
compileCmd hs ftp lbNum (While exp cmd) = ([(OpLb, Label (show lbNum), Null, Null)]
                                       ++ [(OpSc, Null, Null, Null)]
                                       ++ expCode
                                       ++ [(OpIfFalse, UVar expVar, Label (show (lbNum + 1)), Null)]
                                       ++ code
                                       ++ [(OpDeSc True, Null, Null, Null)]
                                       ++ [(OpJp, Label (show lbNum), Null, Null)]
                                       ++ [(OpLb, Label (show (lbNum + 1)), Null, Null)], lbNum1 + 1)
  where (expVar, expCode, _) = compileExp hs 0 exp
        (code, lbNum1) = compileCmd hs ftp (lbNum + 2) cmd
compileCmd hs ftp lbNum (Attr var exp) = (expCode ++ [(OpAt, UVar var, UVar expVar, Null)], lbNum)
  where (expVar, expCode, _) = compileExp hs 0 exp
compileCmd hs ftp lbNum (Print ls) = ((compilePrint hs ls) ++ [(OpPrintLC, Null, Null, Null)], lbNum)
compileCmd hs ftp lbNum (IfCmd iflist) = compileIfList hs ftp lbNum iflist
compileCmd hs ftp lbNum (Ret exp) = (expCode ++ [(OpRet, TVar (expVar, ftp), Null, Null)] ++ [(OpDeSc False, Null, Null, Null)] ++ [(OpJr, Null, Null, Null)], lbNum)
  where (expVar, expCode, _) = compileExp hs 0 exp
compileCmd hs ftp lbNum (Seq cmd1 cmd2) = (code1 ++ code2, lbNum2)
    where (code1, lbNum1) = compileCmd hs ftp lbNum cmd1
          (code2, lbNum2) = compileCmd hs ftp lbNum1 cmd2
compileCmd _ ftp lbNum None = ([], lbNum)

compilePrint :: FuncTable -> [Expr] -> [Code]
compilePrint _ [] = []
compilePrint hs (exp:xs) = expCode ++ [(OpPrint, UVar expVar, Null, Null)] ++ (compilePrint hs xs)
    where (expVar, expCode, _) = compileExp hs 0 exp

compileIfList :: FuncTable -> Type -> Int -> IfList -> ([Code], Int)
compileIfList hs ftp lbNum (If ifArr elseCmd) = (ifsCode
                                          ++ elseCode
                                          ++ [(OpLb, Label (show lbNum), Null, Null)], lbNum2)
    where (ifsCode, lbNum1) = compileIfs hs ftp lbNum ifArr (lbNum + 1)
          (elseCode, lbNum2) = compileCmd hs ftp lbNum1 elseCmd

compileIfs :: FuncTable -> Type -> Int -> [(Expr, Command)] -> Int -> ([Code], Int)
compileIfs hs _ endLb [] lbNum = ([], lbNum)
compileIfs hs ftp endLb (x:xs) lbNum = (expCode
                                        ++ [(OpIfFalse, UVar expVar, Label (show lbNum), Null)]
                                        ++ cmdCode
                                        ++ [(OpJp, Label (show endLb), Null, Null)]
                                        ++ [(OpLb, Label (show lbNum), Null, Null)]
                                        ++ ifsCode, lbNumF)
    where (expVar, expCode, _) = compileExp hs 0 (fst x)
          (cmdCode, lbNum1) = compileCmd hs ftp (lbNum + 1) (snd x)
          (ifsCode, lbNumF) = compileIfs hs ftp endLb xs lbNum1

newVar :: Int -> String
newVar nx = (show nx) ++ "t"

compileExp :: FuncTable -> Int -> Expr -> (String, [Code], Int)
compileExp _ nx (EConst bl) = (var, [(OpAt, UVar var, Const bl, Null)], nx + 1)
    where var = newVar nx
compileExp _ nx (EVar var) = (var, [], nx)
compileExp hs nx (FCall str args) = (var, paramsCode
                                          ++ [(OpCall, TVar (var, fType), UVar str, Null)], nx1 + 1)
  where (fType, argTypes) = fromJust ("function " ++ str ++ " not defined") (HashTable.lookup str hs)
        (paramsCode, nx1) = compileParamsCall hs args argTypes nx 0
        var = newVar nx1
compileExp hs nx (BiOperation exp1 opBi exp2) = (var, cexp1
                                                      ++ cexp2
                                                      ++ [(OpOp opBi, UVar var, UVar var1, UVar var2)], nx2 + 1)
  where (var1, cexp1, nx1) = compileExp hs nx exp1
        (var2, cexp2, nx2) = compileExp hs nx1 exp2
        var = newVar nx2
compileExp hs nx (UnOperation opUn exp1) = (var, cexp1
                                                 ++ [(OpOp opUn, UVar var, UVar var1, Null)], nx1 + 1)
  where (var1, cexp1, nx1) = compileExp hs nx exp1
        var = newVar nx1
