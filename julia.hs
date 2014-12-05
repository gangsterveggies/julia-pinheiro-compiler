module Main where
import Lexer
import Parser

data Ops = BBi BoolOpsBi | BUi BoolOpsUn | Cmp CompOps
data Op = OpAdd | OpMul | OpAt | OpJp | OpLb | OpIfFalse
data Value = Num Int | TVar String | Label String | Null
type Code = (Op, Value, Value, Value)

compileCmd :: Int -> Cmd -> ([Code], Int)
compileCmd lbNum (While exp cmd) = ([(OpLb, Label (show lbNum), Null, Null)]
                                   ++ expCode
                                   ++ [(OpIfFalse, TVar expVar, Label (show (lbNum + 1)), Null)]
                                   ++ code
                                   ++ [(OpJp, Label (show lbNum), Null, Null)]
                                   ++ [(OpLb, Label (show (lbNum + 1)), Null, Null)], lbNum1 + 1)
    where (expCode, expVar, _) = compileExpBool 0 exp
          (code, lbNum1) = compileCmd (lbNum + 2) cmd
compileCmd lbNum (Attr (Var var) exp) = (expCode ++ [(OpAt, TVar var, TVar expVar, Null)], lbNum)
    where (expCode, expVar, _) = compileExp 0 exp
compileCmd lbNum (IfCmd iflist) = (ifcode, lbNum1)
    where (ifCode, lbNum1) = compileIfList lbNum1 iflist
compileCmd lbNum (Seq cmd1 cmd2) = (code1 ++ code2, lbNum2)
    where (code1, lbNum1) = compileCmd lbNum cmd1
          (code2, lbNum2) = compileCmd lbNum1 cmd2
compileCmd lbNum None = ([], lbNum)

compileIfList :: Int -> IfList -> ([Code], Int)
compileIfList lbNum (If ifArr elseCmd) = (ifsCode
                                          ++ [(OpLb, Label (show lbNum1), Null, Null)]
                                          ++ elseCode
                                          ++ [(OpLb, Label (show lbNum), Null, Null)], lbNum2)
    where (ifsCode, lbNum1) = compileIfs lbNum ifArr (lbNum + 1)
          (elsecode, lbNum2) = compileCmd elseCmd (lbNum1 + 1)

compileIfs :: Int -> [(ExprBool, Command)] -> Int -> ([Code], Int)
compileIfs endLb [] lbNum = ([], lbNum)
compileIfs endLb (x:xs) lbNum = (expCode
                                 ++ [(OpIfFalse, TVar expVar, Label (show lbNum), Null)]
                                 ++ cmdCode
                                 ++ [(OpJp, Label (show endLb), Null, Null)]
                                 ++ [(OpLb, Label (show lbNum), Null, Null)]
                                 ++ ifsCode, lbNumF)
    where (expCode, expVar) = compileBoolExp 0 (fst x)
          (cmdCode, lbNum1) = compileCmd (lbNum + 1) (snd x)
          (ifsCode, lbNumF) = compileIfs endLb xs lbNum1

newVar :: Int -> String
newVar nx = (show nx) ++ "t"

getOp :: Ops -> Op

compileExp :: Int -> Expr -> (String, [Code], Int)
compileExp nx (ExprBool exp) = compileExpBool nx exp
compileExp nx (ExprNum exp) = compileExpNum nx exp

compileExpBool :: Int -> ExprBool -> (String, [Code], Int)
compileExpBool nx (BoolConst True) = (var, [(OpAt, TVar var, Num 1, Null)], nx + 1)
    where var = newVar nx
compileExpBool nx (BoolConst False) = (var, [(OpAt, TVar var, Num 0, Null)], nx + 1)
    where var = newVar nx
compileExpBool nx (BoolVar var) = (var, [], nx)
compileExpBool nx (Operation1 exp1 opBi exp2) = cexp1
                                                ++ cexp2
                                                ++ (var, [(getOp(opBi), TVar var, TVar var1, TVar var2)], nx2 + 1)
    where (var1, cexp1, nx1) = compileExpBool nx exp1
          (var2, cexp2, nx2) = compileExpBool nx1 exp2
          var = newVar nx2
compileExpBool nx (Operation2 opUn exp1) = cexp1
                                           ++ (var, [(getOp(opBi), TVar var, TVar var1, Null)], nx1 + 1)
    where (var1, cexp1, nx1) = compileExpBool nx exp1
          var = newVar nx1
compileExpBool nx (Operation3 exp1 opCp exp2) = cexp1
                                                ++ cexp2
                                                ++ (var, [(getOp(opCp), TVar var, TVar var1, TVar var2)], nx2 + 1)
    where (var1, cexp1, nx1) = compileExpNum nx exp1
          (var2, cexp2, nx2) = compileExpNum nx1 exp2
          var = newVar nx2

compileExpNum :: Int -> ExprNum -> (String, [Code], Int)
compileExpNum nx (NumConst num) = (var, [(OpAt, var, Num num, Null, Null)], nx + 1)
    where var = newVar nx
compileExpNum nx (NumVar var) = (var, [], nx)
compileExpNum nx (Operation exp1 opNm exp2) = cexp1
                                              ++ cexp2
                                              ++ (var, [(getOp(opNm), TVar var, TVar var1, TVar var2)], nx2 + 1)
    where (var1, cexp1, nx1) = compileExpNum nx exp1
          (var2, cexp2, nx2) = compileExpNum nx1 exp2
          var = newVar nx2

main = do
  inStr <- getContents
  let parseTree = compileCmd(parse(alexScanTokensWrapper inStr))
  putStrLn (show(parseTree))
  print "done"
