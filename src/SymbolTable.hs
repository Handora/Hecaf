module SymbolTable where

import qualified AST as A
import qualified Data.Map as Map

type EnvStack = [Env]
type Env = Map.Map A.AId Descriptor
data Descriptor = FieldDesc {
                    typeDesc :: TypeDesc
                    }
                | MethodDesc {
                    retType :: TypeDesc,
                    paramTypes :: [TypeDesc]
                    }
                deriving (Show)  
data TypeDesc = PrimitiveDesc A.AType 
              | ArrayDesc A.AType
              deriving (Show)

