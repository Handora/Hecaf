module AST where

-- import Parser 

class Parse2AST a where
  convert :: a -> AST

data AST = AST [ImportNode] [FieldNode] [MethodNode]
         deriving (Show)

data ImportNode = ImportNode AId derving (Show)



