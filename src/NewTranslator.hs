module NewTranslator where

import NewSemantic
import NewAST
import Util



getTypesInContext :: (M,TypeDirectory) -> (Type,Type)
getTypesInContext (m,dir) = mapTuple (getTypeFromName dir) (fst m)

translateToConstruct :: Type -> ConstructKind -> Construct
translateToConstruct  t (CALLSITE m)  = case (getMthdWithName (getMethods_Type t) m) of Just x -> CallSite x
                                                                                        Nothing -> error "Method Not Found"
translateToConstruct  t ALLOCATION  = Allocation (getTypeName t)
translateToConstruct  t CLSDECL = head (getClassDecl t)

translateToChange :: Chng -> C
translateToChange c@(C  s n2)    = if (isTypeChange s) then [(changeType n2 s,c)] else [(changeName n2 s,c)]
translateToChange (Seq c1 c2)  = translateToChange c1 ++ translateToChange c2

renameMethod :: Name -> Mthd -> Mthd
renameMethod n m = (updtSnd.updtFst) (\x -> n) m 

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

changeTypeNamesInOverridenMethod :: TypeName -> Construct -> Construct
changeTypeNamesInOverridenMethod t( MethodDeclaration m)  = MethodDeclaration (t,snd m)
changeTypeNamesInOverridenMethod t _ = error "change type names in method cannot be applied"

changeName ::  Name -> SubConstruct ->  Construct -> Construct
changeName n  Name  (CallSite m)  = CallSite (renameMethod n m)
changeName n  Name  (MethodDeclaration m) = MethodDeclaration (renameMethod n m)
changeName n  (OveridenMthd nm Name) (ClassDecl t ms) = (ClassDecl t  (changeNameOfOverridenMethods n nm ms))
changeName n _ _ = error "ChangeName cannot be applied"


changeType :: TypeName -> SubConstruct -> Construct -> Construct
changeType t To (Allocation t1) = Allocation t
changeType t ReturnType (CallSite m) = CallSite (updtRetType m t)
changeType t Receiver (CallSite m) = CallSite (t,snd m)
changeType t a@(Arg i) (CallSite m) = CallSite (changeArgType t a m)
changeType t SubType (ClassDecl t2 m) = (ClassDecl t (map (changeTypeNamesInOverridenMethod t) m))
changeType t (OveridenMthd n ReturnType) (ClassDecl t2 m) =  (ClassDecl t  (changeRetTypeInOverridenMethod t n m))
changeType t (OveridenMthd n (Param i)) (ClassDecl t2 m) =  (ClassDecl t  (map (\z -> if(getMthdDeclName z == n) then changeParamType t i z else z) m))
changeType t _ _ = error "Operation not supported"

getMthdDeclName (MethodDeclaration m) = getMthdName m

type ResMapping = [((Construct,Maybe Construct),[Chng])]

type Res =( (Type,Type),ResMapping)
