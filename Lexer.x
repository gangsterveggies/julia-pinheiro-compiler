{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    -- Methods
  read                                  { \p s -> TokenRead p }
  println                               { \p s -> TokenPrintln p }
    -- Attributions
  \=                                    { \p s -> TokenAtr p }
    -- Expressions
  \(                                    { \p s -> TokenLB p }
  \)                                    { \p s -> TokenRB p }
  [\+\-\*\/\^\%\!\|\&\=\<\>]+           { \p s -> TokenOp p s }
    -- Ifs
  if                                    { \p s -> TokenIf p }
  elseif                                { \p s -> TokenElseIf p }
  else                                  { \p s -> TokenElse p }
    -- While
  while                                 { \p s -> TokenWhile p }
    -- Functions
  return                                { \p s -> TokenReturn p }
  function                              { \p s -> TokenDef p }
    -- Types
  int                                   { \p s -> TokenTInt p }
  float                                 { \p s -> TokenTFloat p }
  bool                                  { \p s -> TokenTBool p }
    -- Miscelaneous
  \,                                    { \p s -> TokenComma p }
  end                                   { \p s -> TokenEnd p }
  \;                                    { \p s -> TokenSep p }
  \n                                    { \p s -> TokenLC p }
    -- Types and Variables
  $digit+				{ \p s -> TokenInt p (read s) }
  $digit+\.$digit			{ \p s -> TokenFloat p (read s) }
  true                                  { \p s -> TokenBool p True }
  false                                 { \p s -> TokenBool p False }
  $alpha [$alpha $digit \_ !]*		{ \p s -> TokenVar p s }

  [\ \t\f\v\r]+				;
{

data Token = TokenInt AlexPosn Int -- Types and Variables
           | TokenFloat AlexPosn Float
           | TokenBool AlexPosn Bool
     	   | TokenVar AlexPosn String
             -- Arithmetic Expressions
           | TokenLB AlexPosn -- (
           | TokenRB AlexPosn -- )
           | TokenOp AlexPosn String
             -- Methods
           | TokenRead AlexPosn
           | TokenPrintln AlexPosn
             -- Attributions
           | TokenAtr AlexPosn -- =
             -- Ifs
           | TokenIf AlexPosn
           | TokenElseIf AlexPosn
           | TokenElse AlexPosn
             -- While
           | TokenWhile AlexPosn
             -- Functions
           | TokenReturn AlexPosn
           | TokenDef AlexPosn
             -- Types
           | TokenTInt AlexPosn
           | TokenTFloat AlexPosn
           | TokenTBool AlexPosn
             -- Miscelaneous
           | TokenComma AlexPosn
           | TokenEnd AlexPosn
           | TokenSep AlexPosn -- ;
           | TokenLC  AlexPosn -- \n
             deriving (Show)

tokenPosn (TokenInt p _) = p
tokenPosn (TokenFloat p _) = p
tokenPosn (TokenBool p _) = p
tokenPosn (TokenVar p _) = p
             -- Arithmetic Expressions
tokenPosn (TokenOp p _) = p
             -- Methods
tokenPosn (TokenRead p) = p
tokenPosn (TokenPrintln p) = p
             -- Attributions
tokenPosn (TokenAtr p) = p
             -- Ifs
tokenPosn (TokenIf p) = p
tokenPosn (TokenElseIf p) = p
tokenPosn (TokenElse p) = p
             -- While
tokenPosn (TokenWhile p) = p
             -- Functions
tokenPosn (TokenReturn p) = p
tokenPosn (TokenDef p) = p
             -- Types
tokenPosn (TokenTInt p) = p
tokenPosn (TokenTFloat p) = p
tokenPosn (TokenTBool p) = p
             -- Miscelaneous
tokenPosn (TokenComma p) = p
tokenPosn (TokenEnd p) = p
tokenPosn (TokenSep p) = p
tokenPosn (TokenLC p) = p

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn offset lineNum colNum) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn offset lineNum colNum) = colNum

trim :: [Token] -> [Token]
trim [] = []
trim ((TokenLC p1):(TokenLC p2):ts) = (trim ((TokenLC p1) : ts))
trim (t:ts) = t : (trim ts)

alexScanTokensWrapper :: String -> [Token]
alexScanTokensWrapper str = trim (go (alexStartPos, '\n', [], str))
    where go inp@(pos, _, _, str) =
              case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error ("lexical error @ line " ++ show (getLineNum(pos)) ++ " and column " ++ show (getColumnNum(pos)))
                AlexSkip inp' len -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

}
