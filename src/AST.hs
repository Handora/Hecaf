module AST where

class Parse2AST a where
  convert :: a -> AST

data AST = A
