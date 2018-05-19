module ASTChecks where
import Data.Char (isSpace)
import AST
import Errors
-- cNE : checkNothingEmpty
isNotBlank :: String -> Bool
isNotBlank s = not (all isSpace s)

typeNamesNotBlank :: (String,String) -> [String]
typeNamesNotBlank (x,y) = if areNotBlank [x,y] then [] else [noBlanks]

areNotBlank :: [String] -> Bool
areNotBlank p = (foldl (&&) True (map isNotBlank p)) 

cNE_Stmt :: Stmt -> Bool
cNE_Stmt (V_D s) = isNotBlank s
cNE_Stmt (M_D mn rt p) = areNotBlank (mn:rt:p)

cNE_Expr :: Expr -> Bool
cNE_Expr (I_d s) = isNotBlank s
cNE_Expr (M_I mn rt p) = areNotBlank (mn:rt:p)

cNE :: Mapping -> [String]
cNE (S ((t1,s1),(t2,s2))) = testCondition (cNE_Stmt s1  && cNE_Stmt s2) noBlanks
cNE (E ((t1,e1),(t2,e2))) = testCondition (cNE_Expr e1  && cNE_Expr e2) noBlanks

testCondition :: Bool -> String -> [String]
testCondition b s = if(b) then [] else [s]

checkEqualParamArg :: Mapping -> [String] 
checkEqualParamArg (S ((t1,(M_D _ _ x)),(t2,M_D _ _ y))) = testCondition ((length x) == (length y)) paramArgNotEqual
checkEqualParamArg (E ((t1,(M_I _ _ x)),(t2,M_I _ _ y))) = testCondition ((length x) == (length y)) paramArgNotEqual
checkEqualParamArg _                                     = []

astChecks :: M -> [String]
astChecks(T (x,y) m) = typeNamesNotBlank (x,y) ++ concat (map cNE m) ++ concat (map checkEqualParamArg m)

