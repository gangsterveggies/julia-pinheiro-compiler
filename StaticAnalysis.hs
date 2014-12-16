module StaticAnalysis where
import Utils
import Data.Char
import Parser
import TCompile
import ScopeMap

type ScopeStack = ScopeMap Type

staticAnalysis :: [Code] -> [Code]
staticAnalysis c = staticAnalysisGo ScopeMap.empty c

staticAnalysisGo :: ScopeStack -> [Code] -> [Code]
staticAnalysisGo _ [] = []
staticAnalysisGo sc (line:xs) = lineCode : (staticAnalysisGo sc1 xs)
  where (lineCode, sc1) = sAnalyse sc line

sAnalyse :: ScopeStack -> Code -> (Code, ScopeStack)
sAnalyse sc (OpAt, UVar var, x, Null) = ((OpAt, TVar (var, tp), convertValue sc x, Null), sc1)
  where tp = getType x sc
        sc1 = fromJust ("cannot convert variable '" ++ var ++ "' to " ++ (show tp)) (setType var tp sc)
sAnalyse sc (OpSc, Null, Null, Null) = ((OpSc, Null, Null, Null), ScopeMap.enscope sc True)
sAnalyse sc (OpSc, UVar s, Null, Null) = ((OpSc, UVar s, Null, Null), ScopeMap.enscope sc False)
sAnalyse sc (OpDeSc True, Null, Null, Null) = ((OpDeSc True, Null, Null, Null), ScopeMap.descope sc)
sAnalyse sc (OpDeSc False, Null, Null, Null) = ((OpDeSc False, Null, Null, Null), sc)
sAnalyse sc (OpPrint, UVar var, Null, Null) = ((OpPrint, TVar (var, tp), Null, Null), sc)
  where tp = getType (UVar var) sc
sAnalyse sc (OpIfFalse, UVar var, lb, Null) = ((OpIfFalse, TVar (var, tp), lb, Null), sc)
  where tp = getType (UVar var) sc
sAnalyse sc (OpParam ct bl, TVar (var, tp), Null, Null) = ((OpParam ct bl, TVar (var, tp), Null, Null), sc1)
  where sc1 = fromJust ("variable '" ++ var ++ "' called in function as " ++ (show tp)) (setType var tp sc)
sAnalyse sc (OpCall, TVar (var, tp), vl, Null) = ((OpCall, TVar (var, tp), vl, Null), sc1)
  where sc1 = fromJust ("variable '" ++ var ++ "' set to return value of type " ++ (show tp)) (setType var tp sc)
sAnalyse sc (OpRet, TVar (var, tp), Null, Null) = ((OpRet, TVar (var, tp), Null, Null), sc1)
  where sc1 = fromJust ("variable '" ++ var ++ "' set as return value of type " ++ (show tp)) (setType var tp sc)
sAnalyse sc (OpOp op, UVar var, y, z)
  | isArithmetic op = sArithmetic sc (OpOp op, UVar var, y, z)
  | isBoolExp op = sBoolExp sc (OpOp op, UVar var, y, z)
  | otherwise = ((OpOp op, UVar var, y, z), sc)
sAnalyse sc code = (code, sc)

getType :: Value -> ScopeStack -> Type
getType (Const x) _ = convertType x
getType (UVar var) sc = fromJust ("variable " ++ var ++ " not defined") (ScopeMap.lookup var sc)

setType :: String -> Type -> ScopeStack -> Maybe ScopeStack
setType var tp sc
  | varV == Nothing = Just (ScopeMap.insert var tp sc)
  | isRegister var = Just (ScopeMap.insert var tp sc)
  | otherwise = if (fromJust "" varV) == tp then Just sc else Nothing
  where varV = ScopeMap.lookup var sc

sArithmetic :: ScopeStack -> Code -> (Code, ScopeStack)
sArithmetic sc (op, UVar var, y, z) = ((op, TVar (var, tp1), convertValue sc y, convertValue sc z), sc2)
  where tp1 = getType y sc
        tp2 = getType z sc
        sc1 = fromJust ("cannot convert variable '" ++ var ++ "' to " ++ (show tp1)) (setType var tp1 sc)
        sc2 = fromJust ("invalid operation with " ++ (show tp1) ++ " and " ++ (show tp2)) (setType var tp2 sc1)

sBoolExp :: ScopeStack -> Code -> (Code, ScopeStack)
sBoolExp sc (op, UVar var, y, z) = fromJust"" (testAndError (tp1 == tp2 && tp1 /= TBool) ("invalid operation with " ++ (show tp1) ++ " and " ++ (show tp2)) ((op, TVar (var, TBool), convertValue sc y, convertValue sc z), sc1))
  where tp1 = getType y sc
        tp2 = getType z sc
        sc1 = fromJust ("cannot convert variable '" ++ var ++ "' to " ++ (show TBool)) (setType var TBool sc)

convertType :: ValueType -> Type
convertType (VTBool _) = TBool
convertType (VTInt _) = TInt
convertType (VTFloat _) = TFloat

convertValue :: ScopeStack -> Value -> Value
convertValue sc (UVar var) = (TVar (var, tp))
  where tp = getType (UVar var) sc
convertValue _ vl = vl

isRegister :: String -> Bool
isRegister = isDigit . head

isArithmetic :: String -> Bool
isArithmetic str
  | str == "+" = True
  | str == "-" = True
  | str == "*" = True
  | str == "/" = True
  | str == "%" = True
  | otherwise = False

isBoolExp :: String -> Bool
isBoolExp str
  | str == "==" = True
  | str == "!=" = True
  | str == "<" = True
  | str == ">" = True
  | str == "<=" = True
  | str == ">=" = True
  | otherwise = False
