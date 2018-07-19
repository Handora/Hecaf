module Configuration.Types where

--------------------------- The configuration type ----------------------------

data Configuration = Configuration { input :: FilePath
                                   , explicitTarget :: Maybe CompilerStage
                                   , debug :: Bool
                                   , opt :: OptimizationSpecification
                                   , outputFileName :: Maybe FilePath
                                   } deriving (Eq)

defaultConfiguration :: Configuration
defaultConfiguration = Configuration { input = undefined
                                     , explicitTarget = Nothing
                                     , debug = False
                                     , opt = Some [] -- no optimizations
                                     , outputFileName = Nothing
                                     }


------------------------------- Compiler stages -------------------------------

data CompilerStage = Scan
                   | Parse
                   | Inter
                   | Assembly
                   deriving (Eq, Ord)
instance Show CompilerStage where
  show Scan = "scan"
  show Parse = "parse"
  show Inter = "inter"
  show Assembly = "assembly"
instance Read CompilerStage where
  readsPrec _ "scan" = [(Scan, "")]
  readsPrec _ "parse" = [(Parse, "")]
  readsPrec _ "inter" = [(Inter, "")]
  readsPrec _ "assembly" = [(Assembly, "")]
  readsPrec _ _ = []


-------------------------- Describing optimizations ---------------------------

data OptimizationSpecification = All
                               | Some [OptimizationName]
                               deriving (Eq)

-- String might be the wrong type to use here, but whatever.
data OptimizationName = Enable String
                      | Disable String
                      deriving (Eq)
