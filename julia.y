{
module Main where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  "("               { TokenLB _ }

%%
Exp :
            "(" { Cenas 3 }
{

data Cenas = Cenas Int deriving (Show, Eq)

parseError :: [Token] -> a
parseError tokenList = let pos = tokenPosn(head(tokenList)) in
                       error ("parse error at line " ++ show(getLineNum(pos)) ++ " and column " ++ show(getColumnNum(pos)))

main = do
  inStr <- getContents
  let parseTree = alexScanTokensWrapper inStr
  putStrLn ("parseTree: " ++ show(parseTree))
  print "done"
}
