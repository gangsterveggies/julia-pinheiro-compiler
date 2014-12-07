module StaticAnalysis where
import Utils
import Data.Char
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
sAnalyse sc (OpSc, Null, Null, Null) = ((OpSc, Null, Null, Null), ScopeMap.enscope sc)
sAnalyse sc (OpDeSc, Null, Null, Null) = ((OpDeSc, Null, Null, Null), ScopeMap.descope sc)
sAnalyse sc (OpPrint, UVar var, Null, Null) = ((OpPrint, TVar (var, tp), Null, Null), sc)
  where tp = getType (UVar var) sc
sAnalyse sc (op, UVar var, y, z)
  | isArithmetic op = sArithmetic sc (op, UVar var, y, z)
  | otherwise = ((op, UVar var, y, z), sc)
sAnalyse sc code = (code, sc)

convertType :: ValueType -> Type
convertType (VTBool _) = TBool
convertType (VTInt _) = TInt
convertType (VTFloat _) = TFloat

convertValue :: ScopeStack -> Value -> Value
convertValue sc (UVar var) = (TVar (var, tp))
  where tp = getType (UVar var) sc
convertValue _ vl = vl

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

isRegister :: String -> Bool
isRegister = isDigit . head

isArithmetic :: Op -> Bool
isArithmetic OpAdd = True
isArithmetic OpSub = True
isArithmetic OpMul = True
isArithmetic OpDiv = True
isArithmetic OpMod = True
isArithmetic _ = False
