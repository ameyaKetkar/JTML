module ResultChecks where

import Semantic
import Errors
import SemanticFunctions

checkRes :: ResMapping -> TypeDirectory -> Bool
checkRes x dir = foldl (&&) True (map (validResultConstruct dir) (map (\y->(snd (fst y))) x))

validResultConstruct ::TypeDirectory ->  Maybe Construct  -> Bool
validResultConstruct dir (Just (CallSite m)) = if (elem m (getMethods_Type (getTypeFromName dir (fst m)))) then True else error (invalidCallsite m)
validResultConstruct dir (Just (ClassDecl n m1)) = foldl (&&) True (map (\m -> if (elem m (getMethods_Type (getTypeFromName dir n))) then True else error ("MD"++ getMthdName m)) (mthdFrmMthdDecl m1))
validResultConstruct _   (Just _) = True
validResultConstruct _ Nothing     = True
