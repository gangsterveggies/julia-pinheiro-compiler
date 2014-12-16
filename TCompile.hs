module TCompile where
import Utils
import Parser

import qualified Data.Map as HashTable

type HashTable a = HashTable.Map String a
type FuncTable = HashTable (Type, [Type])
data Op = OpOp String | OpFunc | OpParam Int Bool | OpCall | OpRet | OpEnd | OpJr
        | OpAt | OpJp | OpLb | OpIfFalse | OpPrint | OpPrintLC | OpRd
        | OpSc | OpDeSc Bool deriving (Show)
data Value = Const ValueType | UVar String | Label String | TVar (String, Type) | Null deriving (Show)
type Code = (Op, Value, Value, Value)

compile :: [Func] -> [Code]
compile fs = (OpCall, TVar ("0t", TInt), UVar main, Null) : (OpEnd, Null, Null, Null) : (compileFuncs fMap hs fs1)
  where (fs1, fMap) = renameFuncs HashTable.empty 1 fs
        hs = buildFuncMap fs1
        main = fromJust ("function main not defined") (HashTable.lookup "main" fMap)

renameFuncs :: HashTable String -> Int -> [Func] -> ([Func], HashTable String)
renameFuncs fMap _ [] = ([], fMap)
renameFuncs fMap vl ((Func (nm, tp1) ls cmd):fs) = ((Func (fname, tp1) ls cmd) : funcs, fMap1)
  where fname = "F" ++ (show vl)
        (funcs, fMap1) = renameFuncs (HashTable.insert nm fname fMap) (vl + 1) fs

buildFuncMap :: [Func] -> FuncTable
buildFuncMap [] = HashTable.empty
buildFuncMap ((Func (nm, tp1) ls _):fs) = HashTable.insert nm (tp1, getArgTypes ls) (buildFuncMap fs)

getArgTypes :: [(String, Type)] -> [Type]
getArgTypes [] = []
getArgTypes ((_, tp):xs) = tp : (getArgTypes xs)

compileFuncs :: HashTable String -> FuncTable -> [Func] -> [Code]
compileFuncs _ _ [] = []
compileFuncs fMap hs (f:fs) = (compileFunc fMap hs f) ++ (compileFuncs fMap hs fs)

compileFunc :: HashTable String -> FuncTable -> Func -> [Code]
compileFunc fMap hs (Func nm args cmd) = [(OpSc, UVar "0j", Null, Null)]
                                         ++ [(OpFunc, TVar nm, Null, Null)]
                                         ++ paramsCode
                                         ++ code
                                         ++ [(OpDeSc True, Null, Null, Null)]
  where paramsCode = compileParamsDef args 0
        code = compileCmds fMap hs (snd nm) cmd

compileParamsDef :: [(String, Type)] -> Int -> [Code]
compileParamsDef [] _ = []
compileParamsDef (x:xs) curPar = [(OpParam curPar True, TVar x, Null, Null)] ++ (compileParamsDef xs (curPar + 1))

compileParamsCall :: HashTable String -> FuncTable -> [Expr] -> [Type] -> Int -> Int -> ([Code], Int)
compileParamsCall _ _ [] _ nx _ = ([], nx)
compileParamsCall fMap hs (x:xs) (tp:ts) nx curPar = (expCode
                                              ++ [(OpParam curPar False, TVar (var, tp), Null, Null)]
                                              ++ paramsCode, nx2)
  where (var, expCode, nx1) = compileExp fMap hs nx x
        (paramsCode, nx2) = (compileParamsCall fMap hs xs ts nx1 (curPar + 1))

compileCmds :: HashTable String -> FuncTable -> Type -> Command -> [Code]
compileCmds fMap hs ftp cmd = (fst (compileCmd fMap hs ftp 0 cmd))

compileCmd :: HashTable String -> FuncTable -> Type -> Int -> Command -> ([Code], Int)
compileCmd fMap hs ftp lbNum (While exp cmd) = ([(OpLb, Label (show lbNum), Null, Null)]
                                       ++ [(OpSc, Null, Null, Null)]
                                       ++ expCode
                                       ++ [(OpIfFalse, UVar expVar, Label (show (lbNum + 1)), Null)]
                                       ++ code
                                       ++ [(OpDeSc True, Null, Null, Null)]
                                       ++ [(OpJp, Label (show lbNum), Null, Null)]
                                       ++ [(OpLb, Label (show (lbNum + 1)), Null, Null)], lbNum1 + 1)
  where (expVar, expCode, _) = compileExp fMap hs 0 exp
        (code, lbNum1) = compileCmd fMap hs ftp (lbNum + 2) cmd
compileCmd fMap hs ftp lbNum (Attr var exp) = (expCode ++ [(OpAt, UVar var, UVar expVar, Null)], lbNum)
  where (expVar, expCode, _) = compileExp fMap hs 0 exp
compileCmd fMap hs ftp lbNum (Print ls) = ((compilePrint fMap hs ls) ++ [(OpPrintLC, Null, Null, Null)], lbNum)
compileCmd fMap hs ftp lbNum (IfCmd iflist) = compileIfList fMap hs ftp lbNum iflist
compileCmd fMap hs ftp lbNum (Ret exp) = (expCode ++ [(OpRet, TVar (expVar, ftp), Null, Null)] ++ [(OpDeSc False, Null, Null, Null)] ++ [(OpJr, Null, Null, Null)], lbNum)
  where (expVar, expCode, _) = compileExp fMap hs 0 exp
compileCmd fMap hs ftp lbNum (Seq cmd1 cmd2) = (code1 ++ code2, lbNum2)
    where (code1, lbNum1) = compileCmd fMap hs ftp lbNum cmd1
          (code2, lbNum2) = compileCmd fMap hs ftp lbNum1 cmd2
compileCmd _ _ ftp lbNum None = ([], lbNum)

compilePrint :: HashTable String -> FuncTable -> [Expr] -> [Code]
compilePrint _ _ [] = []
compilePrint fMap hs (exp:xs) = expCode ++ [(OpPrint, UVar expVar, Null, Null)] ++ (compilePrint fMap hs xs)
    where (expVar, expCode, _) = compileExp fMap hs 0 exp

compileIfList :: HashTable String -> FuncTable -> Type -> Int -> IfList -> ([Code], Int)
compileIfList fMap hs ftp lbNum (If ifArr elseCmd) = (ifsCode
                                          ++ elseCode
                                          ++ [(OpLb, Label (show lbNum), Null, Null)], lbNum2)
    where (ifsCode, lbNum1) = compileIfs fMap hs ftp lbNum ifArr (lbNum + 1)
          (elseCode, lbNum2) = compileCmd fMap hs ftp lbNum1 elseCmd

compileIfs :: HashTable String -> FuncTable -> Type -> Int -> [(Expr, Command)] -> Int -> ([Code], Int)
compileIfs fMap hs _ endLb [] lbNum = ([], lbNum)
compileIfs fMap hs ftp endLb (x:xs) lbNum = (expCode
                                             ++ [(OpIfFalse, UVar expVar, Label (show lbNum), Null)]
                                             ++ cmdCode
                                             ++ [(OpJp, Label (show endLb), Null, Null)]
                                             ++ [(OpLb, Label (show lbNum), Null, Null)]
                                             ++ ifsCode, lbNumF)
    where (expVar, expCode, _) = compileExp fMap hs 0 (fst x)
          (cmdCode, lbNum1) = compileCmd fMap hs ftp (lbNum + 1) (snd x)
          (ifsCode, lbNumF) = compileIfs fMap hs ftp endLb xs lbNum1

newVar :: Int -> String
newVar nx = (show nx) ++ "t"

compileExp :: HashTable String -> FuncTable -> Int -> Expr -> (String, [Code], Int)
compileExp _ _ nx (EConst bl) = (var, [(OpAt, UVar var, Const bl, Null)], nx + 1)
    where var = newVar nx
compileExp _ _ nx (EVar var) = (var, [], nx)
compileExp fMap hs nx (FCall str1 args) = (var, paramsCode
                                          ++ [(OpCall, TVar (var, fType), UVar str, Null)], nx1 + 1)
  where str = fromJust ("function " ++ str1 ++ " not defined") (HashTable.lookup str1 fMap)
        (fType, argTypes) = fromJust ("function " ++ str ++ " not defined") (HashTable.lookup str hs)
        (paramsCode, nx1) = compileParamsCall fMap hs args argTypes nx 0
        var = newVar nx1
compileExp _ _ nx (Read tp) = (var, [(OpRd, TVar (var, tp), Null, Null)], nx + 1)
  where var = newVar nx
compileExp fMap hs nx (BiOperation exp1 opBi exp2) = (var, cexp1
                                                      ++ cexp2
                                                      ++ [(OpOp opBi, UVar var, UVar var1, UVar var2)], nx2 + 1)
  where (var1, cexp1, nx1) = compileExp fMap hs nx exp1
        (var2, cexp2, nx2) = compileExp fMap hs nx1 exp2
        var = newVar nx2
compileExp fMap hs nx (UnOperation opUn exp1) = (var, cexp1
                                                 ++ [(OpOp opUn, UVar var, UVar var1, Null)], nx1 + 1)
  where (var1, cexp1, nx1) = compileExp fMap hs nx exp1
        var = newVar nx1
