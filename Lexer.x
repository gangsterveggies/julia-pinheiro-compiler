{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    -- Arithmetic Expressions
  \+                                    { \p s -> TokenAdd p }
  \-                                    { \p s -> TokenSub p }
  \*                                    { \p s -> TokenMul p }
  \/                                    { \p s -> TokenDiv p }
  \^                                    { \p s -> TokenPow p }
  \%                                    { \p s -> TokenMod p }
  \(                                    { \p s -> TokenLB p }
  \)                                    { \p s -> TokenRB p }
    -- Boolean Expressions
  \!                                    { \p s -> TokenBolNot p }
  \|\|                                  { \p s -> TokenBolAnd p }
  \&\&                                  { \p s -> TokenBolOr p }
    -- Boolean Comparisons
  \=\=                                  { \p s -> TokenEq p }
  \!\=                                  { \p s -> TokenNotEq p }
  \<                                    { \p s -> TokenLss p }
  \>                                    { \p s -> TokenGrt p }
  \<\=                                  { \p s -> TokenLeq p }
  \>\=                                  { \p s -> TokenGeq p }
    -- Methods
  println                               { \p s -> TokenPrintln p }
    -- Attributions
  \=                                    { \p s -> TokenAtr p }
    -- Ifs
  if                                    { \p s -> TokenIf p }
  elseif                                { \p s -> TokenElseIf p }
  else                                  { \p s -> TokenElse p }
    -- While
  while                                 { \p s -> TokenWhile p }
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
           | TokenAdd AlexPosn -- +
     	   | TokenSub AlexPosn -- -
           | TokenMul AlexPosn -- *
           | TokenDiv AlexPosn -- /
           | TokenPow AlexPosn -- ^
           | TokenMod AlexPosn -- %
           | TokenLB AlexPosn  -- (
           | TokenRB AlexPosn -- )
             -- Boolean Expressions
           | TokenBolNot AlexPosn -- !
           | TokenBolAnd AlexPosn -- &&
           | TokenBolOr AlexPosn -- ||
             -- Boolean Comparisons
           | TokenEq AlexPosn     -- ==
           | TokenNotEq AlexPosn  -- !=
           | TokenLss AlexPosn    -- <
           | TokenGrt AlexPosn    -- >
           | TokenLeq AlexPosn    -- <=
           | TokenGeq AlexPosn    -- >=
             -- Methods
           | TokenPrintln AlexPosn
             -- Attributions
           | TokenAtr AlexPosn -- =
             -- Ifs
           | TokenIf AlexPosn
           | TokenElseIf AlexPosn
           | TokenElse AlexPosn
             -- While
           | TokenWhile AlexPosn
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
tokenPosn (TokenAdd p) = p
tokenPosn (TokenSub p) = p
tokenPosn (TokenMul p) = p
tokenPosn (TokenDiv p) = p
tokenPosn (TokenPow p) = p
tokenPosn (TokenMod p) = p
tokenPosn (TokenLB p) = p
tokenPosn (TokenRB p) = p
             -- Boolean Expressions
tokenPosn (TokenBolNot p) = p
tokenPosn (TokenBolAnd p) = p
tokenPosn (TokenBolOr p) = p
             -- Boolean Comparisons
tokenPosn (TokenEq p) = p
tokenPosn (TokenNotEq p) = p
tokenPosn (TokenLss p) = p
tokenPosn (TokenGrt p) = p
tokenPosn (TokenLeq p) = p
tokenPosn (TokenGeq p) = p
             -- Methods
tokenPosn (TokenPrintln p) = p
             -- Attributions
tokenPosn (TokenAtr p) = p
             -- Ifs
tokenPosn (TokenIf p) = p
tokenPosn (TokenElseIf p) = p
tokenPosn (TokenElse p) = p
             -- While
tokenPosn (TokenWhile p) = p
             -- Miscelaneous
tokenPosn (TokenComma p) = p
tokenPosn (TokenEnd p) = p
tokenPosn (TokenSep p) = p
tokenPosn (TokenLC p) = p

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn offset lineNum colNum) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn offset lineNum colNum) = colNum

alexScanTokensWrapper :: String -> [Token]
alexScanTokensWrapper str = go (alexStartPos,'\n',[],str)
    where go inp@(pos,_,_,str) =
              case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error ("lexical error @ line " ++ show (getLineNum(pos)) ++ " and column " ++ show (getColumnNum(pos)))
                AlexSkip inp' len -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

}
