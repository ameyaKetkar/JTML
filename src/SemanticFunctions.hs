module SemanticFunctions where

import AST
import Semantic
import Util


getTypeFromName :: TypeDirectory -> TypeName -> Type
getTypeFromName (x:xs) t= case ((getTypeName x) == t) of True -> x
                                                         False-> getTypeFromName xs t
getTypeFromName [] t = error "Type Not found in directory "

getMethods_Type :: Type -> [Mthd]
getMethods_Type (Ty m) = snd m
getMethods_Type (SubTy t) = getMethods_Type t

getMthdWithName :: [Mthd] -> Name -> Maybe Mthd
getMthdWithName [] n = Nothing
getMthdWithName (m:ms) n = if(getMthdName m == n) then Just m else getMthdWithName ms n

getClassDecl t = [ClassDecl (getTypeName t) (map (\m ->MethodDeclaration m) (getMethods_Type t))]
getAllocations t = [Allocation (getTypeName t)]
getMethods t =  map (\x -> CallSite x) (getMethods_Type t)

generatePossibleConstructs :: Type -> [Construct]
generatePossibleConstructs t = getClassDecl t ++ getMethods t 


getAllMethodsInDir :: TypeDirectory -> [Mthd]
getAllMethodsInDir dir = concat (map getMethods_Type dir)

removeMethodFrmCls :: (Construct -> Bool)  -> Construct -> Construct
removeMethodFrmCls f (ClassDecl n m ) = (ClassDecl n (removeElem f m) )
removeMethodFrmCls _ c = c


matchMethods :: [String] -> Construct -> Bool
matchMethods xs (MethodDeclaration m) = (getMthdName m)`elem` xs
matchMethods xs _ = False


getMthdDeclName  :: Construct -> Maybe String
getMthdDeclName (MethodDeclaration m) = Just (getMthdName m)
getMthdDeclName m                     = Nothing

getMthdsFromCls :: Construct -> [String]
getMthdsFromCls (ClassDecl n m) =  getNameOfMthdDecl m
getMthdsFromCls      _          =  []

getNameOfMthdDecl :: [Construct] -> [String]
getNameOfMthdDecl ((MethodDeclaration (_,(n,x))):xs) = n:(getNameOfMthdDecl xs)
getNameOfMthdDecl (_:xs) = (getNameOfMthdDecl xs)
getNameOfMthdDecl []     = []

mthdFrmMthdDecl :: [Construct] -> [Mthd]
mthdFrmMthdDecl ((MethodDeclaration m):cs) = (m: (mthdFrmMthdDecl cs))
mthdFrmMthdDecl [] = []

