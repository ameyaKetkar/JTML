module Translator where

import Semantic
import SemanticFunctions
import AST
import Util
import ManageChange

getTypesInContext :: (M,TypeDirectory) -> (Type,Type)
getTypesInContext (m,dir) = mapTuple (getTypeFromName dir) (fst m)

translateToConstruct :: Type -> ConstructKind -> [Construct]
translateToConstruct  t (CALLSITE ms)  = map (\m -> case (getMthdWithName (getMethods_Type t) m) of Just x -> CallSite x
                                                                                                    Nothing -> error "Method Not Found") ms
translateToConstruct  t ALLOCATION  = [Allocation (getTypeName t)]
translateToConstruct  t CLSDECL = [head (getClassDecl t)]

translateToChange :: Chng -> C
translateToChange c@(C  s n2)    = if (isTypeChange s) then [(changeType n2 s,c)] else [(changeName n2 s,c)]
translateToChange (Seq c1 c2)  = translateToChange c1 ++ translateToChange c2
translateToChange e@(E s f)    =  if (isTypeChange s) then error "cannot change types with patterns" else [(editName f s,e)]

