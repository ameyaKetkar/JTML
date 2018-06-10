module ManageChange where

import AST
import Semantic
import Util
import Data.Maybe


renameMethod :: Name -> Mthd -> Mthd
renameMethod n m = (updtSnd.updtFst) (\x -> n) m


editMethodName :: (String -> String)->Mthd ->  Mthd
editMethodName f m= (updtSnd.updtFst) f m

updtRetType :: Mthd -> TypeName -> Mthd
updtRetType (t1,(n,(t2,x))) t =   (t1,(n,(t,x)))


changeRetType :: TypeName -> Mthd -> Mthd
changeRetType tn m =  (updtSnd.updtSnd.updtFst) (\x -> tn) m

changeArgType :: TypeName -> SubConstruct -> Mthd -> Mthd
changeArgType t (Arg i) m =  (updtSnd.updtSnd.updtSnd) (map (\z -> (\x -> if ((fst x) == i) then (updtSnd (\y -> t) x) else x)z)) m


changeParamType :: TypeName -> Int -> Construct ->Construct
changeParamType t  i (MethodDeclaration m) =  MethodDeclaration ((updtSnd.updtSnd.updtSnd) (map (\z -> (\x -> if ((fst x) == i) then (updtSnd (\y -> t) x) else x)z)) m)


changeRetTypeInOverridenMethod :: TypeName -> Name -> [Construct] -> [Construct]
changeRetTypeInOverridenMethod tn n (x@(MethodDeclaration m):ms) = if (getMthdName m == n ) then
  ((MethodDeclaration (changeRetType tn m)):ms) else (x:(changeRetTypeInOverridenMethod tn n ms))
changeRetTypeInOverridenMethod tn n []  = []

changeNameOfOverridenMethods :: Name -> Name -> [Construct] -> [Construct]
changeNameOfOverridenMethods nw mn (x@(MethodDeclaration m):ms) = if (getMthdName m == mn) then
  ((MethodDeclaration (renameMethod nw m)):ms) else (x:(changeNameOfOverridenMethods nw mn ms))
changeNameOfOverridenMethods tn n []  = []

editNameOfOverridenMethods :: (Name -> Name) -> Name -> [Construct] -> [Construct]
editNameOfOverridenMethods nw mn (x@(MethodDeclaration m):ms) = if (getMthdName m == mn) then
  ((MethodDeclaration (editMethodName nw m)):ms) else (x:(editNameOfOverridenMethods nw mn ms))
editNameOfOverridenMethods tn n []  = []

changeTypeNamesInOverridenMethod :: TypeName -> Construct -> Construct
changeTypeNamesInOverridenMethod t( MethodDeclaration m)  = MethodDeclaration (t,snd m)
changeTypeNamesInOverridenMethod t _ = error "change type names in method cannot be applied"

changeName ::  Name -> SubConstruct ->  Construct -> Construct
changeName n  Name  (CallSite m)  = CallSite (renameMethod n m)
changeName n  Name  (MethodDeclaration m) = MethodDeclaration (renameMethod n m)
changeName n  (OveridenMthd nm Name) (ClassDecl t ms) = (ClassDecl t  (changeNameOfOverridenMethods n nm ms))
changeName n _ _ = error "ChangeName cannot be applied"

editName ::  (Name -> Name) -> SubConstruct ->  Construct -> Construct
editName n   Name  (CallSite m)  = CallSite (editMethodName n m)
editName n   Name  (MethodDeclaration m) = MethodDeclaration (editMethodName n m)
editName n  (OveridenMthd nm Name) (ClassDecl t ms) = (ClassDecl t  (editNameOfOverridenMethods n nm ms))
editName n _ _ = error "ChangeName cannot be applied"


changeType :: TypeName -> SubConstruct -> Construct -> Construct
changeType t To (Allocation t1) = Allocation t
changeType t ReturnType (CallSite m) = CallSite (updtRetType m t)
changeType t Receiver (CallSite m) = CallSite (t,snd m)
changeType t a@(Arg i) (CallSite m) = CallSite (changeArgType t a m)
changeType t SubType (ClassDecl t2 m) = (ClassDecl t (map (changeTypeNamesInOverridenMethod t) m))
changeType t (OveridenMthd n ReturnType) (ClassDecl t2 m) =  (ClassDecl t  (changeRetTypeInOverridenMethod t n m))
changeType t (OveridenMthd n (Param i)) (ClassDecl t2 m) =  (ClassDecl t  (map (\z -> if(getMthdDeclName1 z == n) then changeParamType t i z else z) m))
changeType t _ _ = error "Operation not supported"



applySeq :: C -> Construct -> Construct
applySeq (x:xs) c = applySeq xs ((fst x) c)
applySeq [] c = c


getChanges :: C -> [Chng]
getChanges xs = map (\x -> snd x) xs

chngForOvrdnMthd :: Chng -> Maybe String
chngForOvrdnMthd (C (OveridenMthd n _ ) _ ) = Just n
chngForOvrdnMthd _ = Nothing

getChngdOvrdnMthd :: [Chng] -> [String]
getChngdOvrdnMthd a = catMaybes (map (\x -> chngForOvrdnMthd x) a)

getDontMigrateMthdName :: [Chng] -> [String]
getDontMigrateMthdName ((DONT_MIGRAtE s):ms) = (s:(getDontMigrateMthdName ms))
getDontMigrateMthdName (_:ms) = (getDontMigrateMthdName ms)
getDontMigrateMthdName [] = []
