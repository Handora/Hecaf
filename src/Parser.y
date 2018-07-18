{
module Parser ( parse
              ) where

import Text.Printf (printf)

import Scanner (ScannedToken(..), Token(..))

}


--------------------------------- Directives ----------------------------------

%name parse
%error { parseError }
%monad { Either String }

%tokentype { ScannedToken }

%token
  int_t      { ScannedToken _ _ (DataType "int") }
  bool_t     { ScannedToken _ _ (DataType "bool") }
  string     { ScannedToken _ _ (StringLiteral $$) }
  char       { ScannedToken _ _ (CharLiteral $$) }  
  break      { ScannedToken _ _ Break }
  "import"   { ScannedToken _ _ Import }
  continue   { ScannedToken _ _ Continue }
  "if"       { ScannedToken _ _ If }
  "else"     { ScannedToken _ _ Else }
  for        { ScannedToken _ _ For }
  while      { ScannedToken _ _ While }
  return     { ScannedToken _ _ Return }
  len        { ScannedToken _ _ Len }
  identifier { ScannedToken _ _ (Identifier $$) }
  "{"        { ScannedToken _ _ LCurly }
  "}"        { ScannedToken _ _ RCurly }
  ";"        { ScannedToken _ _ (Sym ";") }
  "["        { ScannedToken _ _ LBracket }
  "]"        { ScannedToken _ _ RBracket }
  "("        { ScannedToken _ _ LParenthes }
  ")"        { ScannedToken _ _ RParenthes }
  int        { ScannedToken _ _ (IntLiteral $$) }
  bool       { ScannedToken _ _ (BoolLiteral $$) }
  ","        { ScannedToken _ _ (Sym ",") }
  void       { ScannedToken _ _ Void }
  "++"       { ScannedToken _ _ (Sym "++") }
  "--"       { ScannedToken _ _ (Sym "--") }
  "+="       { ScannedToken _ _ (Sym "+=") }
  "-="       { ScannedToken _ _ (Sym "-=") }
  "-"        { ScannedToken _ _ (Sym "-") }
  "+"        { ScannedToken _ _ (Sym "+") }
  "="        { ScannedToken _ _ (Sym "=") }
  "/"        { ScannedToken _ _ (Sym "/") }
  "*"        { ScannedToken _ _ (Sym "*") }
  "!"        { ScannedToken _ _ (Sym "!") }  
  "%"        { ScannedToken _ _ (Sym "%") }
  "&&"       { ScannedToken _ _ (Sym "&&") }
  "||"       { ScannedToken _ _ (Sym "||") }
  "?"        { ScannedToken _ _ (Sym "?") }
  ":"        { ScannedToken _ _ (Sym ":") }
  "=="       { ScannedToken _ _ (Sym "==") }
  "!="       { ScannedToken _ _ (Sym "!=") }
  ">"        { ScannedToken _ _ (Sym ">") }
  "<"        { ScannedToken _ _ (Sym "<") }
  ">="       { ScannedToken _ _ (Sym ">=") }
  "<="       { ScannedToken _ _ (Sym "<=") }

%right "?" ":"
%left "||"
%left "&&"
%nonassoc "==" "!="
%nonassoc ">" "<" ">=" "<="
%left "+" "-"
%left "*" "/" "%"
%left "!"
%left NEG

%% -------------------------------- Grammar -----------------------------------
Program : ImportDecls FieldsDecls MethodDecls { Program $1 $2 $3 }

ImportDecl : "import" identifier ";" { ImportDecl $2 }

FieldsDecl : Type CommaDecls ";" { FieldsDecl $1 $2 }

FieldDecl : identifier { VarDecl $1 }
          | identifier "[" int "]" { ArrayDecl $1 $3 }

CommaDecls : FieldDecl { [$1] }
           | FieldDecl "," CommaDecls { $1 : $3 }
           

MethodDecl : Type identifier "(" ParamDecls ")" Block { TMethodDecl $1 $2 $4 $6 }
           | Type identifier "(" ")" Block { TMethodDecl $1 $2 [] $5 } 
           | void identifier "(" ParamDecls ")" Block { VMethodDecl $2 $4 $6 }
           | void identifier "(" ")" Block { VMethodDecl $2 [] $5 }

ParamDecl : Type identifier { ParamDecl $1 $2 }

ParamDecls : ParamDecl { [$1] }
           | ParamDecl "," ParamDecls { $1 : $3 }

Block : "{" FieldsDecls Statements "}" { Block $2 $3 }

Statement : Location AssignExpr ";" { AssignStatement $1 $2 }
          | MethodCall ";" { MethodCallStatement $1 }
          | "if" "(" Expr ")" Block { IfStatement $3 $5 Nothing }
          | "if" "(" Expr ")" Block "else" Block { IfStatement $3 $5 (Just $7) }
          | for "(" identifier "=" Expr ";" Expr ";" Location VarChanged ")" Block
              { ForStatement $3 $5 $7 $9 $10 $12 }
          | while "(" Expr ")" Block { WhileStatement $3 $5 }
          | return ";" { ReturnStatement Nothing }
          | return Expr ";" { ReturnStatement $ Just $2 }
          | break ";" { BreakStatement }
          | continue ";" { ContinueStatement }

Location : identifier { VarLocation $1 }
         | identifier "[" Expr "]" { ArrayLocation $1 $3 }         

AssignExpr : "+=" Expr { HInc $2 }
           | "-=" Expr { HDec $2 }
           | "=" Expr { HAssign $2 }
           | "++" { HInc1 }
           | "--" { HDec1 }

MethodCall : identifier "(" MethodArgs ")" { MethodCall $1 $3 }
           | identifier "(" ")" { MethodCall $1 [] }

MethodArgs : MethodArg { [$1] }
           | MethodArg "," MethodArgs { $1 : $3 }

MethodArg : Expr { MethodArg $1 }

VarChanged : "+=" Expr { VInc $2 }
           | "-=" Expr { VDec $2 }
           | "++" { VInc1 }
           | "--" { VDec1 }

Statements :  { [] }
           | Statement Statements { $1 : $2 }

ImportDecls :  { [] }
            | ImportDecl ImportDecls { $1 : $2 }

FieldsDecls :  { [] }
            | FieldsDecls FieldsDecl { $1 ++ [$2] }

MethodDecls :  { [] }
            | MethodDecl MethodDecls { $1 : $2 }

Type : int_t { HInt }
     | bool_t { HBool }

     
Expr : Location { LocExpr $1 }
     | MethodCall { CallExpr $1 }
     | int { IntExpr $1 }
     | bool { BoolExpr $1 }
     | string { StringExpr $1 }
     | char { CharExpr $1 }
     | len identifier { LenExpr $2 }
     | "(" Expr ")" { CuryExpr $2 }
     | "-" Expr %prec NEG { NegExpr $2 }
     | "!" Expr { NotExpr $2 }
     | Expr "||" Expr { OrExpr $1 $3 }
     | Expr "&&" Expr { AndExpr $1 $3 }
     | Expr "==" Expr { EqualExpr $1 $3 }
     | Expr "!=" Expr { NotEqualExpr $1 $3 }
     | Expr "<" Expr { LessExpr $1 $3 }
     | Expr ">" Expr { LargerExpr $1 $3 }
     | Expr "<=" Expr { LessThanExpr $1 $3 }
     | Expr ">=" Expr { LargerThanExpr $1 $3 }
     | Expr "+" Expr { AddExpr $1 $3 }
     | Expr "-" Expr { MinusExpr $1 $3 }
     | Expr "*" Expr { MultipleExpr $1 $3 }
     | Expr "/" Expr { DivideExpr $1 $3 }
     | Expr "%" Expr { ModuloExpr $1 $3 }
     | Expr "?" Expr ":" Expr { TrinaryExpr $1 $3 $5 }
     

----------------------------------- Haskell -----------------------------------

{
data Program = Program [ImportDecl] [FieldsDecl] [MethodDecl]
               deriving (Show)
data ImportDecl = ImportDecl String
                  deriving (Show)
data FieldsDecl = FieldsDecl HType [FieldDecl] deriving (Show)
data FieldDecl = VarDecl String
               | ArrayDecl String String
               deriving (Show)
data MethodDecl = TMethodDecl HType String [ParamDecl] Block
                | VMethodDecl String [ParamDecl] Block
                deriving (Show)
data ParamDecl = ParamDecl HType String deriving (Show)

data Block = Block [FieldsDecl] [Statement] deriving (Show)

data Statement = AssignStatement Location AssignExpr
               | MethodCallStatement MethodCall
               | IfStatement Expr Block (Maybe Block)
               | ForStatement String Expr Expr Location VarChanged Block
               | WhileStatement Expr Block
               | ReturnStatement (Maybe Expr)
               | BreakStatement
               | ContinueStatement
               deriving (Show)

data Location = VarLocation String 
              | ArrayLocation String Expr
              deriving (Show)

data AssignExpr = HInc Expr
                | HDec Expr
                | HAssign Expr
                | HInc1
                | HDec1
                deriving (Show)

data MethodCall = MethodCall String [MethodArg]
                deriving (Show)

data VarChanged = VDec Expr
                | VInc Expr
                | VDec1
                | VInc1
                deriving (Show)

data MethodArg = MethodArg Expr
               deriving (Show)

data HType = HInt
          | HBool
          deriving (Show)

data Expr = TrinaryExpr Expr Expr Expr
          | OrExpr Expr Expr
          | AndExpr Expr Expr
          | EqualExpr Expr Expr
          | NotEqualExpr Expr Expr
          | LessExpr Expr Expr
          | LessThanExpr Expr Expr
          | LargerExpr Expr Expr
          | LargerThanExpr Expr Expr
          | AddExpr Expr Expr
          | MinusExpr Expr Expr
          | MultipleExpr Expr Expr
          | DivideExpr Expr Expr
          | ModuloExpr Expr Expr
          | NegExpr Expr
          | NotExpr Expr
          | LocExpr Location
          | CallExpr MethodCall
          | IntExpr String
          | BoolExpr Bool
          | StringExpr String
          | LenExpr String
          | CharExpr Char
          | CuryExpr Expr
          deriving (Show)

parseError :: [ScannedToken] -> Either String a
parseError [] = Left "unexpected EOF"
parseError toks =
  Left $ printf "line %d:%d: unexpected token%s '%s'"
                lineNo
                columnNo
                (if (not $ null $ tail toks) then "s" else "")
                badTokenText
  where firstBadToken = head toks
        lineNo = Scanner.line firstBadToken
        columnNo = Scanner.column firstBadToken
        badTokenText = concatMap (show . extractRawToken) toks

}
