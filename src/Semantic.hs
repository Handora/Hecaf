module Semantic where

import qualified SymbolTable as ST
import qualified AST as A
import qualified Data.List as L
import qualified Data.Map as Map
import Prelude hiding (traverse)

doCheck :: A.AST -> [String]
doCheck (A.AST as) = snd $ L.foldl' check' ([Map.empty], []) as

check' :: (ST.EnvStack, [String]) -> A.ANode -> (ST.EnvStack, [String])
check' (envs, errs) node =
  let (nenvs, nerrs) = traverse node envs
  in (nenvs, errs ++ nerrs)

traverse :: A.ANode -> ST.EnvStack -> (ST.EnvStack, [String])
traverse (A.AMemberDecl (A.AMethodDecl ))
