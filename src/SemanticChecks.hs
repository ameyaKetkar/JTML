module SemanticChecks where
import Data.Char (isSpace)
import AST
import Semantics
import Data.List (elem)
import Errors

typeContainsMthd :: (Type, Mthd) ->  String
typeContainsMthd ((Ty (TD tn m)),m1) = if(m1 `elem` m) then "" else (mthdNotFound  (getMthdName m1))
typeContainsMthd ((SubTy t), m1)     = typeContainsMthd (t,m1)

typesContainMthds ::TypeDirectory -> MappingSem  ->  [String]
typesContainMthds dir m =  case (getMthd m dir) of Just (x,y) -> [typeContainsMthd x,typeContainsMthd y]
                                                   Nothing -> []

testCondition :: Bool -> String -> [String]
testCondition b s = if(b) then [] else [s]

getMthd :: MappingSem -> TypeDirectory -> Maybe ((Type,Mthd),(Type,Mthd))
getMthd (SSem ((t1,MD m1),(t2,MD m2))) dir = Just ((getTypeFromName t1 dir ,m1),(getTypeFromName t2 dir,m2))
getMthd (ESem ((rt1,MI t1  m1),(rt2,MI t2 m2))) dir = Just ((getTypeFromName t1 dir,m1),(getTypeFromName t2 dir,m2))
getMthd _  dir = Nothing

semChecks :: Msem -> TypeDirectory-> [String]
semChecks (Tsem (x,y) m) dir =  concatMap (typesContainMthds dir) m
