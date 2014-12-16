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
  bool                                  { TokenBool _ $$ }
  var                                   { TokenVar _ $$ }
    -- Arithmetic Expressions
  '+'                                   { TokenOp _ "+" }
  '-'                                   { TokenOp _ "-" }
  '*'                                   { TokenOp _ "*" }
  '/'                                   { TokenOp _ "/" }
  '%'                                   { TokenOp _ "%" }
  '('                                   { TokenLB _ }
  ')'                                   { TokenRB _ }
    -- Boolean Expressions
  '!'                                   { TokenOp _ "!" }
  '||'                                  { TokenOp _ "||" }
  '&&'                                  { TokenOp _ "&&" }
    -- Boolean Comparisons
  '=='                                  { TokenOp _ "==" }
  '!='                                  { TokenOp _ "!=" }
  '<'                                   { TokenOp _ "<" }
  '>'                                   { TokenOp _ ">" }
  '<='                                  { TokenOp _ "<=" }
  '>='                                  { TokenOp _ ">=" }
    -- Expressions
  sign                                  { TokenOp _ $$ }
    -- Functions
  def                                   { TokenDef _ }
  return                                { TokenReturn _ }
    -- Types
  tint                                  { TokenTInt _ }
  tfloat                                { TokenTFloat _ }
  tbool                                 { TokenTBool _ }
    -- Methods
  read                                  { TokenRead _ }
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
%left sign
%left lc ';' ','

%%

FList   : Func lc FList                            { $1 : $3 }
        | Func                                     { [$1] }
        | {- empty -}                              { [] }

Func    : def var '(' TArgList ')' Type lc Cmd end { Func ($2, $6) $4 $8 }

TArgList: Type var ',' TArgList                    { ($2, $1) : $4 }
        | Type var                                 { [($2, $1)] }
        | {- empty -}                              { [] }

ListExp : Exp ',' ListExp                          { $1 : $3 }
        | Exp                                      { [$1] }
        | {- empty -}                              { [] }

Type    : tint                                     { TInt }
        | tfloat                                   { TFloat }
        | tbool                                    { TBool }
          
Cmd     : if Exp lc Cmd IfE end                    { IfCmd (If (($2, $4) : $5) None) }
        | if Exp lc Cmd IfE else Cmd end           { IfCmd (If (($2, $4) : $5) $7) }
        | while Exp lc Cmd end                     { While $2 $4 }
        | return Exp                               { Ret $2 }
        | var '=' Exp                              { Attr $1 $3 }
        | println '(' ListExp ')'                  { Print $3 }
        | Cmd lc Cmd                               { Seq $1 $3 }
        | Cmd lc                                   { $1 }
        | lc Cmd                                   { $2 }
        | Cmd ';' Cmd                              { Seq $1 $3 }
        | Cmd ';'                                  { $1 }

IfE     : elseif Exp lc Cmd IfE                    { ($2, $4) : $5 }
        | elseif Exp lc Cmd                        { [($2, $4)] }
        | {- empty -}                              { [] }

Const   : int                                      { EConst (VTInt $1) }
        | float                                    { EConst (VTFloat $1) }
        | bool                                     { EConst (VTBool $1) }

Exp     : Exp sign Exp                             { BiOperation $1 $2 $3 }
        | Exp '+' Exp                              { BiOperation $1 "+" $3 }
        | Exp '-' Exp                              { BiOperation $1 "-" $3 }
        | Exp '*' Exp                              { BiOperation $1 "*" $3 }
        | Exp '/' Exp                              { BiOperation $1 "/" $3 }
        | Exp '%' Exp                              { BiOperation $1 "%" $3 }
        | Exp '||' Exp                             { BiOperation $1 "||" $3 }
        | Exp '&&' Exp                             { BiOperation $1 "&&" $3 }
        | Exp '==' Exp                             { BiOperation $1 "==" $3 }
        | Exp '!=' Exp                             { BiOperation $1 "!=" $3 }
        | Exp '<' Exp                              { BiOperation $1 "<" $3 }
        | Exp '>' Exp                              { BiOperation $1 ">" $3 }
        | Exp '<=' Exp                             { BiOperation $1 "<=" $3 }
        | Exp '>=' Exp                             { BiOperation $1 ">=" $3 }
        | sign Exp                                 { UnOperation $1 $2 }
        | '!' Exp                                  { UnOperation "!" $2 }
        | '(' Exp ')'                              { $2 }
        | var '(' ListExp ')'                      { FCall $1 $3 }
        | read '(' Type ')'                        { Read $3 }
        | Const                                    { $1 }
        | var                                      { EVar $1 }

{

data Type = TInt | TFloat | TBool deriving (Show, Eq)
data ValueType = VTInt Int | VTFloat Float | VTBool Bool deriving (Show)

data Expr = EConst ValueType
          | BiOperation Expr String Expr
          | UnOperation String Expr
          | FCall String [Expr]
          | Read Type
          | EVar String deriving Show

data IfList = If [(Expr, Command)] Command deriving Show

data Command = IfCmd IfList
             | While Expr Command
             | Attr String Expr
             | Ret Expr
             | Print [Expr]
             | Seq Command Command
             | None deriving Show

data Func = Func (String, Type) [(String, Type)] Command deriving Show

parseError :: [Token] -> a
parseError (token:tokenList) = let pos = tokenPosn(token) in
                       error ("parse error at line " ++ show(getLineNum(pos)) ++ " and column " ++ show(getColumnNum(pos)))
parseError _ = error "parse error"
}
