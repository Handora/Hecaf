module SymbolTable where

import qualified AST as A
import qualified Data.Map as Map
import Prelude hiding (lookup)

type EnvStack = [Env]
type Env = Map.Map String (A.APos, Descriptor)
data Descriptor = FieldDesc {
                    fieldType :: TypeDesc
                    }
                | MethodDesc {
                    retType :: A.AType,
                    paramTypes :: [A.AType]
                    }
                | ParamDesc {
                    paramType :: A.AType
                    }
                | ImportDesc  
                deriving (Show)  
data TypeDesc = PrimitiveDesc A.AType 
              | ArrayDesc A.AType
              deriving (Show)

lookup :: String -> EnvStack -> Maybe (A.APos, Descriptor)
lookup id (env:rest) =
  case Map.lookup id env of
    Just res -> Just res
    Nothing -> lookup id rest
lookup id [] = Nothing    
