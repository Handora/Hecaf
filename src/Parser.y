-- Parser -- Decaf parser                                       -*- haskell -*-
-- Copyright (C) 2013  Benjamin Barenblat <bbaren@mit.edu>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- decafc is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the X11 license for more details.
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
  "class"    { ScannedToken _ _ Class }
  int_t      { ScannedToken _ _ (DataType "int") }
  bool_t     { ScannedToken _ _ (DataType "bool") }
  string     { ScannedToken _ _ (StringLiteral $$) }
  break      { ScannedToken _ _ Break }
  "import"   { ScannedToken _ _ Import }
  continue   { ScannedToken _ _ Continue }
  "if"       { ScannedToken _ _ If }
  "else"     { ScannedToken _ _ Else }
  for        { ScannedToken _ _ For }
  while      { ScannedToken _ _ While }
  return     { ScannedToken _ _ Return }
  break      { ScannedToken _ _ Break }
  continue   { ScannedToken _ _ Continue }
  len        { ScannedToken _ _ Len }
  void       { ScannedToken _ _ Void }
  identifier { ScannedToken _ _ (Identifier $$) }
  "{"        { ScannedToken _ _ LCurly }
  "}"        { ScannedToken _ _ RCurly }
  ";"        { ScannedYoken _ _ (Sym ";") }
  "["        { ScannedToken _ _ LBracket }
  "]"        { ScannedToken _ _ RBracket }
  "("        { ScannedToken _ _ LParenthes }
  ")"        { ScannedToken _ _ RParenthes }
  int        { ScannedToken _ _ (IntLiteral $$) }
  bool       { ScannedToken _ _ (BoolLiteral $$) }
  ","        { ScannedYoken _ _ (Sym ",") }
  void       { ScannedYoken _ _ Void }
  "++"       { ScannedYoken _ _ (Sym "++") }
  "--"       { ScannedYoken _ _ (Sym "--") }
  "+="       { ScannedYoken _ _ (Sym "+=") }
  "-="       { ScannedYoken _ _ (Sym "-=") }
  "-"        { ScannedYoken _ _ (Sym "-") }
  "+"        { ScannedYoken _ _ (Sym "+") }
  "="        { ScannedYoken _ _ (Sym "=") }
  "/"        { ScannedYoken _ _ (Sym "/") }
  "%"        { ScannedYoken _ _ (Sym "%") }
  "&&"        { ScannedYoken _ _ (Sym "&&") }
  "||"        { ScannedYoken _ _ (Sym "||") }


%% -------------------------------- Grammar -----------------------------------

Program : ImportDecls FieldsDecls MethodDecls { Program $1 $2 $3 }

ImportDecl : "import" identifier ";" { ImportDecl $2 }

FieldsDecl : Type CommaDecls ";" { FieldsDecl $1 $2 }

FieldDecl : identifier { VarDecl $1 }
          | identifier "[" int "]" { ArrayDecl $1 $3 }

CommaDecls : FieldDecl { [$1] }
           | FieldDecl "," CommaDecls { $1 : $3 }
           | FieldDecl "," { [$1] }

MethodDecl : Type identifier "(" ParamDecls ")" Block { TMethodDecl $1 $2 $4 $6 }
           | Void identifier "(" ParamDecls ")" Block { VMethodDecl $2 $4 $6 }

ParamDecl : Type identifier { ParamDecl $1 $2 }

ParamDecls : ParamDecl { [$1] }
           | ParamDecl "," ParamDecls { $1 : $3 }

Block : "{" FieldDecls Statements "}" { Field $2 $ 3 }

Statement : Location AssignExpr ";" { AssignStatement $1 $2 }
          | MethodCall ";" { MethodCallStatement $1 $2 }
          | "if" "(" Expr ")" Block { IfStatement $3 $5 Nothing }
          | "if" "(" Expr ")" Block else Block { IfStatement $3 $5 (Just $7) }
          | for "(" identifier "=" Expr ";" Expr ";" VarChange ")" Block
              { ForStatement $3 $5 $7 $9 $11 }
          | while "(" Expr ")" Block { WhileStatement $3 $5 }
          | return ";" { ReturnStatement Nothing }
          | return Expr ";" { ReturnStatement $ Just $2 }
          | break ";" { BreakStatement }
          | continue ";" { ContinueStatement }

Location : identifier { VarLocation $1 }
         | identifier "[" Expr "]" { ArrayLocation $1 $3 }         

AssignExpr : "+=" Expr { HInc $2 }
           | "-=" Expr { HDec $2 }
           | "-" Expr { HAssign $2 }
           | "++" { HInc1 }
           | "--" { HDec1 }

MethodCall : identifier "(" MethodArgs ")" { MethodCall $1 $3 }

MethodArgs : MethodArg { [$1] }
           | MethodArg "," MethodArgs { $1 : $3 }

MethodArg : Expr { MExpr $1 }
          | string { MString $1 }

VarChanged : "+=" Expr { VInc $2 }
           | "-=" Expr { VDec $2 }
           | "++" { VInc1 }
           | "--" { VDec1 }

Statements : {- empty -} { [] }
           | Statement Statements { $1 : $2 }

ImportDecls : {- empty -} { [] }
            | ImportDecl ImportDecls { $1 : $2 }

FieldsDecls : {- empty -} { [] }
            | FieldsDecl FieldsDecls { $1 : $2 }

MethodDecls : {- empty -} { [] }
            | MethodDecl MethodDecls { $1 : $2 }

Type : int_t { HInt }
     | bool_t { HBool }

-- Expr : Loction { ELoc $1 }
--      | MethodCall { ECall $1 }
--      | int { EILit $1 }
--      | bool { EBLit $1 }
--      | string { ESLit $1 }
--      | len "(" identifier ")" { ELen $3 }
--      | Expr
Expr0 : Expr0 "?" Expr0 ":" Expr0 { TernaryExpr $1 $3 $5}
      | Expr1 { Expr1 $1 }

Expr1 : Expr1 "||" Expr2 { OrExpr $1 $3 }
      | Expr2 { Expr2 $1 }

Expr2 : Expr2 "&&" Expr3 { AndExpr $1 $3 }
      | Expr3 { Expr3 $1 }
      
Expr3 : Expr3 "==" Expr4 { EqualExpr $1 $3 }
      | Expr3 "!=" Expr4 { NotEqualExpr $1 $3 }
      | Expr4 { Expr4 $1 }

Expr4 : Expr4 "<" Expr5 { LessExpr $1 $3 }
      | Expr4 ">" Expr5 { LargerExpr $1 $3 }
      | Expr4 "<=" Expr5 { LessThanExpr $1 $3 }
      | Expr4 ">=" Expr5 { LargerThanExpr $1 $3 }
      | Expr5 { Expr5 $1 }

Expr5 : Expr5 "+" Expr6 { AddExpr $1 $3 }
      | Expr5 "-" Expr6 { MinusExpr $1 $3 }
      | Expr6 { Expr6 $1 }

Expr6 : Expr6 "*" Expr7 { MutipleExpr $1 $3 }
      | Expr6 "/" Expr7 { DivideExpr $1 $3 }
      | Expr6 "%" Expr7 { ModuloExpr $1 $3 }
      | Expr7 { Expr7 $1 }

Expr7 : "-" Expr7 { NegExpr $2 }
      | "!" Expr7 { NotExpr $2 }
      | Expr8 { Expr8 $1 }

Expr8 : Location { LocExpr $1 }
      | MethodCall { CallExpr $1 }
      | int { IntExpr $1 }
      | bool { BoolExpr $1 }
      | string { StringExpr $1 }
      | len Expr0 { LenExpr $2 }
      | "(" Expr0 ")" { CuryExpr %2 }

----------------------------------- Haskell -----------------------------------

data Program = Program [ImportDecl] [FieldsDecl] [MethodDecl] deriving (Show)
data ImportDecl = ImportDecl String deriving (Show)
data FieldsDecl = FieldsDecl HType [FieldDecl] deriving (Show)
data FieldDecl = VarDecl String
               | ArrayDecl String Int
               deriving (Show)
data MethodDecl = TMethodDecl HType String [ParamDecl] Block
                | VMethodDecl String [ParamDecl] Block
                deriving (Show)
data ParamDecl = ParamDecl HType String deriving (Show)

data Block = Block [FieldsDecl] [Statement] deriving (Show)

data Statement = AssignStatement Location AssignExpr
               | MethodCallStatement MethodCall
               | IfStatement Expr Block (Maybe Block)
               | ForStatement Expr Expr Location VarChanged Block
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

data MethodCall = MethodCall [MethodArg]
                | deriving (Show)

data VarChanged = VDec Expr
                | VInc Expr
                | VDec1
                | VInc1
                deriving (Show)

data MethodArg = MExpr Expr
               | MString String
               deriving (Show)

-- newtype MethodDecls = MethodDecls [MethodDecl] deriving (Show)
-- newtype ImportDecls = ImportDecls [ImportDecl] deriving (Show)
-- newtype FieldsDecls = FieldsDecls [FieldsDecl] deriving (Show)  
data HType = HInt
          | HBool
          deriving (Show)
data HIdentifier = HIdentifier String deriving (Show)

data Expr0 = TrinaryExpr Expr0 Expr0 Expr0
           | Expr1 Expr1
           deriving (Show)

data Expr1 = OrExpr Expr1 Expr2
           | Expr2 Expr2
           deriving (Show)

data Expr2 = AndExpr Expr2 Expr3
           | Expr3 Expr3
           deriving (Show)

data Expr3 = EqualExpr Expr3 Expr4
           | NotEqualExpr Expr3 Expr4
           | Expr4 Expr4
           deriving (Show)

data Expr4 = LessExpr Expr4 Expr5
           | LessThenExpr Expr4 Expr5
           | LargerExpr Expr4 Expr5
           | LargerThenExpr Expr4 Expr5
           | Expr5 Expr5
           deriving (Show)

data Expr5 = AddExpr Expr5 Expr6
           | MinusExpr Expr5 Expr6
           | Expr6 Expr6
           deriving (Show)

data Expr6 = MultipleExpr Expr6 Expr7
           | DivideExpr Expr6 Expr7
           | ModuloExpr Expr6 Expr7
           | Expr7 Expr7
           deriving (Show)

data Expr7 = NegExpr Expr7
           | NotExpr Expr7
           | Expr8 Expr8
           deriving (Show)

data Expr8 = LocExpr Location
           | CallExpr MethodCall
           | IntExpr Int
           | BoolExpr Bool
           | StringExpr String
           | LenExpr Expr0
           | CuryExpr Expr0

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
