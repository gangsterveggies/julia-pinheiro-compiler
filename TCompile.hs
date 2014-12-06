module TCompile where
import Lexer
import Parser

data Ops = BBi BoolOpsBi | BUi BoolOpsUn | Cmp CompOps | NOp NumOps deriving (Show)
data Op = OpAdd | OpSub | OpMul | OpDiv | OpPow | OpMod
        | OpAt | OpJp | OpLb | OpIfFalse deriving (Show)
data Value = Num Int | TVar String | Label String | Null deriving (Show)
type Code = (Op, Value, Value, Value)

compileCmd :: Int -> Command -> ([Code], Int)
compileCmd lbNum (While exp cmd) = ([(OpLb, Label (show lbNum), Null, Null)]
                                   ++ expCode
                                   ++ [(OpIfFalse, TVar expVar, Label (show (lbNum + 1)), Null)]
                                   ++ code
                                   ++ [(OpJp, Label (show lbNum), Null, Null)]
                                   ++ [(OpLb, Label (show (lbNum + 1)), Null, Null)], lbNum1 + 1)
    where (expVar, expCode, _) = compileExpBool 0 exp
          (code, lbNum1) = compileCmd (lbNum + 2) cmd
compileCmd lbNum (Attr (Var var) exp) = (expCode ++ [(OpAt, TVar var, TVar expVar, Null)], lbNum)
    where (expVar, expCode, _) = compileExp 0 exp
compileCmd lbNum (IfCmd iflist) = (ifCode, lbNum1)
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
          (elseCode, lbNum2) = compileCmd (lbNum1 + 1) elseCmd

compileIfs :: Int -> [(ExprBool, Command)] -> Int -> ([Code], Int)
compileIfs endLb [] lbNum = ([], lbNum)
compileIfs endLb (x:xs) lbNum = (expCode
                                 ++ [(OpIfFalse, TVar expVar, Label (show lbNum), Null)]
                                 ++ cmdCode
                                 ++ [(OpJp, Label (show endLb), Null, Null)]
                                 ++ [(OpLb, Label (show lbNum), Null, Null)]
                                 ++ ifsCode, lbNumF)
    where (expVar, expCode, _) = compileExpBool 0 (fst x)
          (cmdCode, lbNum1) = compileCmd (lbNum + 1) (snd x)
          (ifsCode, lbNumF) = compileIfs endLb xs lbNum1

newVar :: Int -> String
newVar nx = (show nx) ++ "t"

getOp :: Ops -> Op
getOp (NOp Add) = OpAdd
getOp (NOp Sub) = OpSub
getOp (NOp Mul) = OpMul
getOp (NOp Div) = OpDiv
getOp (NOp Pow) = OpPow
getOp (NOp Mod) = OpMod

compileExp :: Int -> Expr -> (String, [Code], Int)
compileExp nx (ExprBool exp) = compileExpBool nx exp
compileExp nx (ExprNum exp) = compileExpNum nx exp

compileExpBool :: Int -> ExprBool -> (String, [Code], Int)
compileExpBool nx (BoolConst True) = (var, [(OpAt, TVar var, Num 1, Null)], nx + 1)
    where var = newVar nx
compileExpBool nx (BoolConst False) = (var, [(OpAt, TVar var, Num 0, Null)], nx + 1)
    where var = newVar nx
compileExpBool nx (BoolVar (Var var)) = (var, [], nx)
compileExpBool nx (Operation1 exp1 opBi exp2) = (var,
                                                 cexp1
                                                 ++ cexp2
                                                 ++ [(getOp(BBi opBi), TVar var, TVar var1, TVar var2)], nx2 + 1)
    where (var1, cexp1, nx1) = compileExpBool nx exp1
          (var2, cexp2, nx2) = compileExpBool nx1 exp2
          var = newVar nx2
compileExpBool nx (Operation2 opUn exp1) = (var,
                                            cexp1
                                            ++ [(getOp(BUi opUn), TVar var, TVar var1, Null)], nx1 + 1)
    where (var1, cexp1, nx1) = compileExpBool nx exp1
          var = newVar nx1
compileExpBool nx (Operation3 exp1 opCp exp2) = (var,
                                                 cexp1
                                                 ++ cexp2
                                                 ++ [(getOp(Cmp opCp), TVar var, TVar var1, TVar var2)], nx2 + 1)
    where (var1, cexp1, nx1) = compileExpNum nx exp1
          (var2, cexp2, nx2) = compileExpNum nx1 exp2
          var = newVar nx2

getNum :: JNum -> Int
getNum (NumInt a) = a

compileExpNum :: Int -> ExprNum -> (String, [Code], Int)
compileExpNum nx (NumConst num) = (var, [(OpAt, TVar var, Num (getNum num), Null)], nx + 1)
    where var = newVar nx
compileExpNum nx (NumVar (Var var)) = (var, [], nx)
compileExpNum nx (Operation exp1 opNm exp2) = (var,
                                               cexp1
                                               ++ cexp2
                                               ++ [(getOp(NOp opNm), TVar var, TVar var1, TVar var2)], nx2 + 1)
    where (var1, cexp1, nx1) = compileExpNum nx exp1
          (var2, cexp2, nx2) = compileExpNum nx1 exp2
          var = newVar nx2
