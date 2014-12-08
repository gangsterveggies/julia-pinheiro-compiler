{
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  int                                   { TokenInt _ $$ }
  float                                 { TokenFloat _ $$ }
  true                                  { TokenBool _ True }
  false                                 { TokenBool _ False }
  var                                   { TokenVar _ $$ }
    -- Arithmetic Expressions
  '+'                                   { TokenAdd _ }
  '-'                                   { TokenSub _ }
  '*'                                   { TokenMul _ }
  '/'                                   { TokenDiv _ }
  '^'                                   { TokenPow _ }
  '%'                                   { TokenMod _ }
  '('                                   { TokenLB _ }
  ')'                                   { TokenRB _ }
    -- Boolean Expressions
  '!'                                   { TokenBolNot _ }
  '||'                                  { TokenBolAnd _ }
  '&&'                                  { TokenBolOr _ }
    -- Boolean Comparisons
  '=='                                  { TokenEq _ }
  '!='                                  { TokenNotEq _ }
  '<'                                   { TokenLss _ }
  '>'                                   { TokenGrt _ }
  '<='                                  { TokenLeq _ }
  '>='                                  { TokenGeq _ }
-- Methods
  println                               { TokenPrintln _ }
    -- Attributions
  '='                                   { TokenAtr _ }
    -- Ifs
  if                                    { TokenIf _ }
  elseif                                { TokenElseIf _ }
  else                                  { TokenElse _ }
    -- While
  while                                 { TokenWhile _ }
    -- Miscelaneous
  ','                                   { TokenComma _ }
  end                                   { TokenEnd _ }
  ';'                                   { TokenSep _ }
  lc                                    { TokenLC _ }

%nonassoc '>' '<' '<=' '>=' '==' '!=' '!'
%left '||'
%left '&&'
%left '+' '-'
%left '*' '/' '%'
%left '^'
%left lc ';' ','

%%

Cmd     : if ExpBool lc Cmd IfE end          { IfCmd (If (($2, $4) : $5) None) }
        | if ExpBool lc Cmd IfE else Cmd end { IfCmd (If (($2, $4) : $5) $7) }
        | while ExpBool lc Cmd end           { While $2 $4 }
        | Var '=' Exp                        { Attr $1 $3 }
        | println '(' ListExp ')'            { Print $3 }
        | Cmd lc Cmd                         { Seq $1 $3 }
        | Cmd lc                             { $1 }
        | lc Cmd                             { $2 }
        | Cmd ';' Cmd                        { Seq $1 $3 }
        | Cmd ';'                            { $1 }

IfE     : elseif ExpBool lc Cmd IfE          { ($2, $4) : $5 }
        | elseif ExpBool lc Cmd              { [($2, $4)] }
        | {- empty -}                        { [] }

ListExp : Exp ',' ListExp                    { $1 : $3 }
        | Exp                                { [$1] }

Exp     : ExpNum                             { ExprNum $1 }
        | ExpBool                            { ExprBool $1 }

ExpNum  : ExpNum '+' ExpNum                  { Operation $1 Add $3 }
        | ExpNum '-' ExpNum                  { Operation $1 Sub $3 }
        | ExpNum '*' ExpNum                  { Operation $1 Mul $3 }
        | ExpNum '/' ExpNum                  { Operation $1 Div $3 }
        | ExpNum '^' ExpNum                  { Operation $1 Pow $3 }
        | ExpNum '%' ExpNum                  { Operation $1 Mod $3 }
        | '(' ExpNum ')'                     { $2 }
        | int                                { NumConst (NumInt $1) }
        | float                              { NumConst (NumFloat $1) }
        | Var                                { NumVar $1 }

ExpBool : ExpBool '&&' ExpBool               { Operation1 $1 BolAnd $3 }
        | ExpBool '||' ExpBool               { Operation1 $1 BolOr $3 }
        | '!' ExpBool                        { Operation2 BolNot $2 }
        | ExpNum '==' ExpNum                 { Operation3 $1 Eq $3 }
        | ExpNum '!=' ExpNum                 { Operation3 $1 NotEq $3 }
        | ExpNum '<' ExpNum                  { Operation3 $1 Lss $3 }
        | ExpNum '>' ExpNum                  { Operation3 $1 Grt $3 }
        | ExpNum '<=' ExpNum                 { Operation3 $1 Leq $3 }
        | ExpNum '>=' ExpNum                 { Operation3 $1 Geq $3 }
        | '(' ExpBool ')'                    { $2 }
        | Bool                               { BoolConst $1 }
        | Var                                { BoolVar $1 }

Bool    : true                               { True }
        | false                              { False }

Var    : var                                 { Var $1 }

{

data NumOps = Add
            | Sub
            | Mul
            | Div
            | Pow
            | Mod deriving Show

data BoolOpsBi = BolAnd
               | BolOr deriving Show

data BoolOpsUn = BolNot deriving Show

data CompOps = Eq
             | NotEq
             | Lss
             | Grt
             | Leq
             | Geq deriving Show

data Var = Var String deriving Show

data JNum = NumInt Int
          | NumFloat Float deriving Show

data ExprNum = NumConst JNum
             | Operation ExprNum NumOps ExprNum
             | NumVar Var deriving Show

data ExprBool = BoolConst Bool
              | Operation1 ExprBool BoolOpsBi ExprBool
              | Operation2 BoolOpsUn ExprBool
              | Operation3 ExprNum CompOps ExprNum
              | BoolVar Var deriving Show

data Expr = ExprBool ExprBool
          | ExprNum ExprNum deriving Show

data IfList = If [(ExprBool, Command)] Command deriving Show

data Command = IfCmd IfList
             | While ExprBool Command
             | Attr Var Expr
             | Print [Expr]
             | Seq Command Command
             | None deriving Show

parseError :: [Token] -> a
parseError tokenList = let pos = tokenPosn(head(tokenList)) in
                       error ("parse error at line " ++ show(getLineNum(pos)) ++ " and column " ++ show(getColumnNum(pos)))
}
