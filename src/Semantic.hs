module Semantic where

import qualified SymbolTable as ST
import qualified AST as A
import qualified Data.List as L
import qualified Data.Map as Map
import Util
import Prelude hiding (traverse)

doCheck :: A.AST -> [String]
doCheck (A.AST as) = snd $ L.foldl' check' ([Map.empty], []) as

check' :: (ST.EnvStack, [String]) -> A.ANode -> (ST.EnvStack, [String])
check' (envs, errs) node =
  let (nenvs, nerrs) = traverse node envs
  in (nenvs, errs ++ nerrs)

traverse :: A.ANode -> ST.EnvStack -> (ST.EnvStack, [String])
traverse (A.AMemberDecl (A.AMethodDecl rettype aid aparams stmts)) envstack =
  undefined
  where env = Map.insert aid (ST.MethodDesc rettype (L.map fst aparams)) Map.empty
        env' = L.foldl' insert2Env env aparams
        insert2Env env (atype, aid) =
          Map.insert aid (ST.ParamDesc atype) env

type SemanticCheck = A.ANode -> ST.EnvStack -> [String]

-- | Rule 1: No identifier is declared twice in the same scope.
checkNoTwice :: SemanticCheck
checkNoTwice (A.AMemberDecl (A.AImportDecl aid)) envs =
  maybe [] (\(_, _) -> [report' (A.getPos aid) (A.getId aid)]) $ Map.lookup (A.getId aid) (head envs)
checkNoTwice (A.AMemberDecl (A.AFieldDecl _ alocs)) envs =
  let sames = getListSame (map A.getALocId alocs)
      error = getRepeatError (getAllWithString sames alocs)
  in error 
checkNoTwice (A.AMemberDecl (A.AMethodDecl _ aid aparams _)) envs = 
  let error1 = maybe [] (\(_, _) -> [report' (A.getPos aid) (A.getId aid)]) $ Map.lookup (A.getId aid) (head envs)
      error2 = getRepeatError (getParamSame $ map snd aparams)
  in error1 ++ error2    
  where getParamSame :: [A.AId] -> [[A.AId]]
        getParamSame = L.groupBy (\a b -> A.getId a == A.getId b)  
checkNoTwice _ _ = []


-- | Rule 2: No identifier is used before it is declared.
checkdeclareBeforeUse :: SemanticCheck
checkdeclareBeforeUse (A.AExpression (A.ACallExpr (A.AMethodCallExpr aid _ ))) envs =
  case ST.lookup (A.getId aid) envs of
    Nothing -> [report (A.getPos aid) $ (A.getId aid) ++ "No identifier is used before it is declared."]
    Just 
    
  
report :: A.APos -> String -> String
report apos err = "Line " ++ (show $ A.getLine apos) ++ ", Colume " ++ (show $ A.getCol apos) ++ " error: " ++ err

report' :: A.APos -> String -> String
report' apos name = report apos $ name ++ ": No identifier is declared twice in the same scope"

getListSame :: (Eq a) => [a] -> [a]
getListSame as = L.nub $ getListSame' as
  where getListSame' (a:as)
          | a `L.elem` as = a:getListSame' as
          | otherwise = getListSame' as
        getListSame' [] = []  

getAllWithString :: [String] -> [A.ALocation] -> [[A.AId]]
getAllWithString (name:rest) locs = (L.map A.getALocAId (L.filter (\loc -> name == A.getALocId loc) locs)) : getAllWithString rest locs
getAllWithString [] _ =  []

getRepeatError :: [[A.AId]] -> [String]
getRepeatError (ids:rest) = report (A.getPos (head ids))
  ((A.getId (head ids)) ++ ": No identifier is declared twice in the same scope with"
   ++ showIdWithPos (tail ids)) : getRepeatError rest
getRepeatError [] = []

showIdWithPos :: [A.AId] -> String
showIdWithPos (id:rest) = A.getId id ++ "[" ++ (show $ A.getLine (A.getPos id)) ++ (show $ A.getCol (A.getPos id)) ++ "] " ++ showIdWithPos rest
showIdWithPos [] = ""
