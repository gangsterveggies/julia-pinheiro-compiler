module Main where
import Lexer
import Parser

data Op = Add | Mul | Attr
data Value = Num Int | Var String | Null
type Code = (Op, Value, Value, Value)

compileCmd :: Cmd -> [Code]

compileExp :: Expr -> (String, [Code])

main = do
  inStr <- getContents
  let parseTree = compileCmd(parse(alexScanTokensWrapper inStr))
  putStrLn (show(parseTree))
  print "done"
