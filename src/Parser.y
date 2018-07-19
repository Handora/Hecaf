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
  string     { ScannedToken _ _ (StringLiteral _) }
  char       { ScannedToken _ _ (CharLiteral _) }  
  break      { ScannedToken _ _ Break }
  "import"   { ScannedToken _ _ Import }
  continue   { ScannedToken _ _ Continue }
  "if"       { ScannedToken _ _ If }
  "else"     { ScannedToken _ _ Else }
  for        { ScannedToken _ _ For }
  while      { ScannedToken _ _ While }
  return     { ScannedToken _ _ Return }
  len        { ScannedToken _ _ Len }
  identifier { ScannedToken _ _ (Identifier _) }
  "{"        { ScannedToken _ _ LCurly }
  "}"        { ScannedToken _ _ RCurly }
  ";"        { ScannedToken _ _ (Sym ";") }
  "["        { ScannedToken _ _ LBracket }
  "]"        { ScannedToken _ _ RBracket }
  "("        { ScannedToken _ _ LParenthes }
  ")"        { ScannedToken _ _ RParenthes }
  int        { ScannedToken _ _ (IntLiteral _) }
  bool       { ScannedToken _ _ (BoolLiteral _) }
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

ImportDecl : "import" Id ";" { ImportDecl $2 }

FieldsDecl : Type CommaDecls ";" { FieldsDecl $1 $2 }

FieldDecl : Id { VarDecl $1 }
          | Id "[" IInt "]" { ArrayDecl $1 $3 }

CommaDecls : FieldDecl { [$1] }
           | FieldDecl "," CommaDecls { $1 : $3 }

MethodDecl : Type Id "(" ParamDecls ")" Block { TMethodDecl $1 $2 $4 $6 }
           | Type Id "(" ")" Block { TMethodDecl $1 $2 [] $5 } 
           | void Id "(" ParamDecls ")" Block { VMethodDecl $2 $4 $6 }
           | void Id "(" ")" Block { VMethodDecl $2 [] $5 }

ParamDecl : Type Id { ParamDecl $1 $2 }

ParamDecls : ParamDecl { [$1] }
           | ParamDecl "," ParamDecls { $1 : $3 }

Block : "{" FieldsDecls Statements "}" { Block $2 $3 }

Statement : Location AssignExpr ";" { AssignStatement $1 $2 }
          | MethodCall ";" { MethodCallStatement $1 }
          | "if" "(" Expr ")" Block { IfStatement $3 $5 Nothing }
          | "if" "(" Expr ")" Block "else" Block { IfStatement $3 $5 (Just $7) }
          | for "(" Id "=" Expr ";" Expr ";" Location VarChanged ")" Block
              { ForStatement $3 $5 $7 $9 $10 $12 }
          | while "(" Expr ")" Block { WhileStatement $3 $5 }
          | return ";" { ReturnStatement Nothing }
          | return Expr ";" { ReturnStatement $ Just $2 }
          | break ";" { BreakStatement }
          | continue ";" { ContinueStatement }

Location : Id { VarLocation $1 }
         | Id "[" Expr "]" { ArrayLocation $1 $3 }         

AssignExpr : "+=" Expr { HInc $2 }
           | "-=" Expr { HDec $2 }
           | "=" Expr { HAssign $2 }
           | "++" { HInc1 }
           | "--" { HDec1 }

MethodCall : Id "(" MethodArgs ")" { MethodCall $1 $3 }
           | Id "(" ")" { MethodCall $1 [] }

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
            
Type : int_t { TInt $ extractPos $1 }
     | bool_t { TBool $ extractPos $1 }

Id : identifier { HIdentifier (extractPos $1) (extractId $1) }       
IInt : int { HInt (extractPos $1) (extractInt $1) }
IBool : bool { HBool (extractPos $1) (extractBool $1) }                              
IChar : char { HChar (extractPos $1) (extractChar $1) }
IString : string { HString (extractPos $1) (extractString $1) }
     
Expr : Location { LocExpr $1 }
     | MethodCall { CallExpr $1 }
     | IInt { IntExpr $1 }
     | IBool { BoolExpr $1 }
     | IString { StringExpr $1 }
     | IChar { CharExpr $1 }
     | len Id { LenExpr $2 }
     | "(" Expr ")" { CuryExpr $2 }
     | "-" Expr %prec NEG { NegExpr (extractPos $1) $2 }
     | "!" Expr { NotExpr (extractPos $1) $2 }
     | Expr "||" Expr { OrExpr (extractPos $2) $1 $3 }
     | Expr "&&" Expr { AndExpr (extractPos $2) $1 $3 }
     | Expr "==" Expr { EqualExpr (extractPos $2) $1 $3 }
     | Expr "!=" Expr { NotEqualExpr (extractPos $2) $1 $3 }
     | Expr "<" Expr { LessExpr (extractPos $2) $1 $3 }
     | Expr ">" Expr { LargerExpr (extractPos $2) $1 $3 }
     | Expr "<=" Expr { LessThanExpr (extractPos $2) $1 $3 }
     | Expr ">=" Expr { LargerThanExpr (extractPos $2) $1 $3 }
     | Expr "+" Expr { AddExpr (extractPos $2) $1 $3 }
     | Expr "-" Expr { MinusExpr (extractPos $2) $1 $3 }
     | Expr "*" Expr { MultipleExpr (extractPos $2) $1 $3 }
     | Expr "/" Expr { DivideExpr (extractPos $2) $1 $3 }
     | Expr "%" Expr { ModuloExpr (extractPos $2) $1 $3 }
     | Expr "?" Expr ":" Expr { TrinaryExpr (extractPos $2) (extractPos $4) $1 $3 $5 }

----------------------------------- Haskell -----------------------------------
{

data Program = Program [ImportDecl] [FieldsDecl] [MethodDecl]
               deriving (Show)
data ImportDecl = ImportDecl HIdentifier
                  deriving (Show)
data FieldsDecl = FieldsDecl HType [FieldDecl] deriving (Show)
data FieldDecl = VarDecl HIdentifier
               | ArrayDecl HIdentifier HInt
               deriving (Show)
data MethodDecl = TMethodDecl HType HIdentifier [ParamDecl] Block
                | VMethodDecl HIdentifier [ParamDecl] Block
                deriving (Show)
data ParamDecl = ParamDecl HType HIdentifier deriving (Show)

data Block = Block [FieldsDecl] [Statement] deriving (Show)

data Statement = AssignStatement Location AssignExpr
               | MethodCallStatement MethodCall
               | IfStatement Expr Block (Maybe Block)
               | ForStatement HIdentifier Expr Expr Location VarChanged Block
               | WhileStatement Expr Block
               | ReturnStatement (Maybe Expr)
               | BreakStatement
               | ContinueStatement
               deriving (Show)

data Location = VarLocation HIdentifier
              | ArrayLocation HIdentifier Expr
              deriving (Show)

data AssignExpr = HInc Expr
                | HDec Expr
                | HAssign Expr
                | HInc1
                | HDec1
                deriving (Show)

data MethodCall = MethodCall HIdentifier [MethodArg]
                deriving (Show)

data VarChanged = VDec Expr
                | VInc Expr
                | VDec1
                | VInc1
                deriving (Show)

data MethodArg = MethodArg Expr
               deriving (Show)

data HType = TInt Pos
           | TBool Pos
           deriving (Show)

data HIdentifier = HIdentifier Pos String
                 deriving (Show)

data Expr = TrinaryExpr Pos Pos Expr Expr Expr
          | OrExpr Pos Expr Expr
          | AndExpr Pos Expr Expr
          | EqualExpr Pos Expr Expr
          | NotEqualExpr Pos Expr Expr
          | LessExpr Pos Expr Expr
          | LessThanExpr Pos Expr Expr
          | LargerExpr Pos Expr Expr
          | LargerThanExpr Pos Expr Expr
          | AddExpr Pos Expr Expr
          | MinusExpr Pos Expr Expr
          | MultipleExpr Pos Expr Expr
          | DivideExpr Pos Expr Expr
          | ModuloExpr Pos Expr Expr
          | NegExpr Pos Expr
          | NotExpr Pos Expr
          | LocExpr Location
          | CallExpr MethodCall
          | IntExpr HInt
          | BoolExpr HBool
          | StringExpr HString
          | LenExpr HIdentifier
          | CharExpr HChar
          | CuryExpr Expr
          deriving (Show)

data Pos = Pos {
  line :: Int,
  col :: Int
  } deriving (Show)

data HInt = HInt Pos String deriving (Show)
data HBool = HBool Pos Bool deriving (Show)
data HChar = HChar Pos Char deriving (Show)
data HString = HString Pos String deriving (Show)

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

extractPos :: ScannedToken -> Pos
extractPos (ScannedToken l c _) = Pos l c

extractId :: ScannedToken -> String
extractId (ScannedToken _ _ (Identifier id)) = id

extractInt :: ScannedToken -> String
extractInt (ScannedToken _ _ (IntLiteral i)) = i

extractChar :: ScannedToken -> Char
extractChar (ScannedToken _ _ (CharLiteral c)) = c

extractString :: ScannedToken -> String
extractString (ScannedToken _ _ (StringLiteral s)) = s

extractBool :: ScannedToken -> Bool
extractBool (ScannedToken _ _ (BoolLiteral b)) = b

}
