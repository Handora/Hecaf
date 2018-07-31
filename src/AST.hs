module AST where

import qualified Parser as P

convert2AST :: P.Program -> AST
convert2AST (P.Program is fs ms) =
  AST $ (map convert is) ++ (map convert fs) ++ (map convert ms)

class Parse2ANode a where
  convert :: a -> ANode

instance Parse2ANode P.ImportDecl where
  convert (P.ImportDecl (P.HIdentifier pos id)) =
    AMemberDecl $ AImportDecl $ AId (pos2APos pos) id

instance Parse2ANode P.FieldsDecl where
  convert (P.FieldsDecl ht decls) = 
    AMemberDecl $ AFieldDecl (hType2AType ht) (map field2AField decls)

instance Parse2ANode P.MethodDecl where
  convert (P.TMethodDecl ht hi params block) =
    AMemberDecl $ AMethodDecl (hType2AType ht)
      (hidentifier2AId hi) (hparams2Aparams params) (convertBlock block)

instance Parse2ANode P.Statement where
  convert stmt =
    AStatement $ convertStmt stmt

instance Parse2ANode P.Expr where
  convert expr =
    AExpression $ convertExpr expr

data AST = AST [ANode]
         deriving (Show)

data ANode = AExpression AExpression
           | AStatement AStatement
           | AMemberDecl AMemberDecl
           deriving (Show)

data AExpression = ALiteral ALiteral
                 | ACallExpr ACallExpr
                 | ABinopExpr ABinop AExpression AExpression
                 | AUnopExpr AUnaryop AExpression
                 | ATriopExpr ATriop AExpression AExpression AExpression
                 | ALocation ALocation
                 | ALen AId
                 deriving (Show)

data ALocation = AIdentifier AId
               | AArray AId AExpression
               deriving (Show)

getALocId :: ALocation -> String
getALocId (AIdentifier aid) = getId aid
getALocId (AArray aid _) = getId aid 

getALocAId :: ALocation -> AId
getALocAId (AIdentifier aid) = aid
getALocAId (AArray aid _) = aid

data ALiteral = AIntLiteral APos Integer
              | ABoolLiteral APos Bool
              | AStringLiteral APos String
              | ACharLiteral APos Char
              deriving (Show)

data APos = APos {
  getLine :: Int,
  getCol :: Int
  } deriving (Show, Eq, Ord)

data ACallExpr = AMethodCallExpr AId [AExpression]
               deriving (Show)

data ABinop = APlus APos
            | AMinus APos
            | AMultiple APos
            | ADivide APos
            | ARemiander APos
            | ALarger APos
            | ALess APos
            | ALargerEq APos
            | ALessEq APos
            | AEqual APos
            | ANotEqual APos
            | AAnd APos
            | AOr APos
            deriving (Show)

data AUnaryop = ANot APos
              | ANegative APos
              deriving (Show)

data ATriop = AMarkOp APos APos
            deriving (Show)

data AStatement = AAssignStmt AAssignStmt
                | ABreakStmt
                | AContinueStmt
                | AIfStmt AExpression [ANode] [ANode]
                | AForStmt AId AExpression AExpression AAssignStmt [ANode]
                | AWhileStmt AExpression [ANode]
                | AReturnStmt (Maybe AExpression)
                | ACallStmt ACallExpr
                deriving (Show)

data AAssignStmt = AAssignTo ALocation AExpression
                 | AIncrement ALocation AIncrOp
                 | AIncrementWith ALocation AIncrOp AExpression
                 deriving (Show)

data AIncrOp = APlusPlus 
             | AMinusMinus 
             deriving (Show)
  
data AId = AId {
  getPos :: APos,
  getId :: String
  } deriving (Show, Eq, Ord)

data AMemberDecl = AMethodDecl AType AId [(AType, AId)] [ANode]
                 | AFieldDecl AType [ALocation]
                 | AImportDecl AId
                 deriving (Show)

data AType = AInt APos
           | ABool APos
           | AVoid APos
           deriving (Show)

pos2APos :: P.Pos -> APos
pos2APos p = APos (P.line p) (P.col p)

hType2AType :: P.HType -> AType
hType2AType (P.TInt p) = AInt $ pos2APos p

field2AField :: P.FieldDecl -> ALocation
field2AField (P.VarDecl hid) =
  AIdentifier $ (hidentifier2AId hid)
field2AField (P.ArrayDecl hid (P.HInt p num)) =
  AArray (hidentifier2AId hid)
    (ALiteral $ AIntLiteral (pos2APos p) (read num))

hidentifier2AId :: P.HIdentifier -> AId
hidentifier2AId (P.HIdentifier p id) = AId (pos2APos p) id

hparams2Aparams :: [P.ParamDecl] -> [(AType, AId)]
hparams2Aparams params = map hparam2Aparam params
  where hparam2Aparam (P.ParamDecl ht hi) =
          (hType2AType ht, hidentifier2AId hi)

convertBlock :: P.Block -> [ANode]
convertBlock (P.Block fields stmts) =
  (map convert fields) ++ (map convert stmts)

hAssign2AAssign :: P.Location -> P.AssignExpr -> AAssignStmt
hAssign2AAssign loc (P.HAssign e) = AAssignTo (hLoc2ALoc loc) $ convertExpr e
hAssign2AAssign loc (P.HInc e) = AIncrementWith (hLoc2ALoc loc) (APlusPlus) (convertExpr e)
hAssign2AAssign loc (P.HDec e) = AIncrementWith (hLoc2ALoc loc) (AMinusMinus) (convertExpr e)
hAssign2AAssign loc (P.HInc1) = AIncrement (hLoc2ALoc loc) APlusPlus
hAssign2AAssign loc (P.HDec1) = AIncrement (hLoc2ALoc loc) AMinusMinus

hLoc2ALoc :: P.Location -> ALocation
hLoc2ALoc (P.VarLocation hi) = AIdentifier (hidentifier2AId hi)
hLoc2ALoc (P.ArrayLocation hi e) = AArray (hidentifier2AId hi) (convertExpr e)

convertExpr :: P.Expr -> AExpression
convertExpr (P.TrinaryExpr p1 p2 e1 e2 e3) =
  ATriopExpr (AMarkOp (pos2APos p1) (pos2APos p2)) (convertExpr e1) (convertExpr e2) (convertExpr e3)
convertExpr (P.OrExpr p e1 e2) =
  ABinopExpr (AOr (pos2APos p)) (convertExpr e1) (convertExpr e2)
convertExpr (P.AndExpr p e1 e2) =
  ABinopExpr (AAnd (pos2APos p)) (convertExpr e1) (convertExpr e2)
convertExpr (P.EqualExpr p e1 e2) =
  ABinopExpr (AEqual (pos2APos p)) (convertExpr e1) (convertExpr e2)
convertExpr (P.NotEqualExpr p e1 e2) =
  ABinopExpr (ANotEqual (pos2APos p)) (convertExpr e1) (convertExpr e2)  
convertExpr (P.LessExpr p e1 e2) =
  ABinopExpr (ALess (pos2APos p)) (convertExpr e1) (convertExpr e2)
convertExpr (P.LargerExpr p e1 e2) =
  ABinopExpr (ALess (pos2APos p)) (convertExpr e1) (convertExpr e2)  
convertExpr (P.LargerThanExpr p e1 e2) =
  ABinopExpr (ALargerEq (pos2APos p)) (convertExpr e1) (convertExpr e2)
convertExpr (P.LessThanExpr p e1 e2) =
  ABinopExpr (ALessEq (pos2APos p)) (convertExpr e1) (convertExpr e2)
convertExpr (P.AddExpr p e1 e2) =
  ABinopExpr (APlus (pos2APos p)) (convertExpr e1) (convertExpr e2)
convertExpr (P.MinusExpr p e1 e2) =
  ABinopExpr (AMinus (pos2APos p)) (convertExpr e1) (convertExpr e2)
convertExpr (P.MultipleExpr p e1 e2) =
  ABinopExpr (AMultiple (pos2APos p)) (convertExpr e1) (convertExpr e2)
convertExpr (P.DivideExpr p e1 e2) =
  ABinopExpr (ADivide (pos2APos p)) (convertExpr e1) (convertExpr e2)        
convertExpr (P.ModuloExpr p e1 e2) =
  ABinopExpr (ARemiander (pos2APos p)) (convertExpr e1) (convertExpr e2)        
convertExpr (P.NegExpr p e1) =
  AUnopExpr (ANegative (pos2APos p)) (convertExpr e1)
convertExpr (P.NotExpr p e1) =
  AUnopExpr (ANot (pos2APos p)) (convertExpr e1)
convertExpr (P.LenExpr hid) =
  ALen (hidentifier2AId hid)
convertExpr (P.LocExpr loc) =
  ALocation (hLoc2ALoc loc)
convertExpr (P.CallExpr (P.MethodCall hid args)) =
  ACallExpr $ AMethodCallExpr (hidentifier2AId hid) (map hArgs2AArgs args)
convertExpr (P.IntExpr (P.HInt p si)) =
  ALiteral $ AIntLiteral (pos2APos p) (read si)
convertExpr (P.BoolExpr (P.HBool p b)) =
  ALiteral $ ABoolLiteral (pos2APos p) b
convertExpr (P.StringExpr (P.HString p s)) =
  ALiteral $ AStringLiteral (pos2APos p) s
convertExpr (P.CharExpr (P.HChar p c)) =
  ALiteral $ ACharLiteral (pos2APos p) c
convertExpr (P.CuryExpr e) =
  convertExpr e  

hArgs2AArgs :: P.MethodArg -> AExpression
hArgs2AArgs (P.MethodArg e) = convertExpr e

convertStmt :: P.Statement -> AStatement
convertStmt (P.AssignStatement loc ae) =
  AAssignStmt (hAssign2AAssign loc ae)
convertStmt (P.MethodCallStatement (P.MethodCall hid args)) =
  ACallStmt $ AMethodCallExpr (hidentifier2AId hid) (map hArgs2AArgs args)
convertStmt (P.IfStatement e b mb) =
  AIfStmt (convertExpr e) (convertBlock b) (maybe [] convertBlock mb)
convertStmt (P.ForStatement hi e1 e2 loc vc b) =
  AForStmt (hidentifier2AId hi) (convertExpr e1) (convertExpr e2) (convertVarChanged vc loc) (convertBlock b)
convertStmt (P.WhileStatement e b) =
  AWhileStmt (convertExpr e) (convertBlock b)
convertStmt (P.ReturnStatement me) =
  AReturnStmt (me >>= return . convertExpr)
convertStmt (P.BreakStatement) = ABreakStmt
convertStmt (P.ContinueStatement) = AContinueStmt


convertVarChanged :: P.VarChanged -> P.Location -> AAssignStmt
convertVarChanged (P.VDec e) loc = AIncrementWith (hLoc2ALoc loc) AMinusMinus (convertExpr e)
convertVarChanged (P.VInc e) loc = AIncrementWith (hLoc2ALoc loc) APlusPlus (convertExpr e)
convertVarChanged (P.VDec1) loc = AIncrement (hLoc2ALoc loc) AMinusMinus
convertVarChanged (P.VInc1) loc = AIncrement (hLoc2ALoc loc) APlusPlus
