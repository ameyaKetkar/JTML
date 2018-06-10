module SyntaxChecks where
import AST
import Semantic
import Data.Maybe

validateSyntax :: (M,TypeDirectory) -> Bool
validateSyntax i = (foldl (&&) True   (map validateLCTransform ((snd.fst) i ))) && validateTypeNames i


validateLCTransform :: LCTransform -> Bool
validateLCTransform (c, (C s n)) = validSubConstructs (c,s)
validateLCTransform (c, (Seq c1 c2)) = validateLCTransform (c,c1) && validateLCTransform (c,c2)
validateLCTransform _                = True

validSubConstructs :: (ConstructKind, SubConstruct ) -> Bool
validSubConstructs (ALLOCATION,To) = True
validSubConstructs (ALLOCATION,_) = error "Invalid allocation subconstruct "
validSubConstructs (CALLSITE s,Name) = True
--validSubConstructs (CALLSITE s,ReturnType) = True
validSubConstructs (CALLSITE s,(Arg i)) = True
validSubConstructs (CALLSITE s,Receiver) = True
validSubConstructs (CALLSITE s,MethodInvc) = True
validSubConstructs (CALLSITE s,ReturnType) = True
validSubConstructs (CALLSITE s,_) = error ("Invalid callsite subconstruct ... " ++ concat s)
validSubConstructs (CLSDECL ,SubType) = True
validSubConstructs (CLSDECL ,o@(OveridenMthd s c)) = validSubSubConstructs o
validSubConstructs (CLSDECL ,_) = error " Invalid Class declaration subconstruct"

validSubSubConstructs :: SubConstruct -> Bool
validSubSubConstructs (OveridenMthd s (Param i)) = True
validSubSubConstructs (OveridenMthd s (Name)) = True
validSubSubConstructs (OveridenMthd s (ReturnType)) = True
validSubSubConstructs (OveridenMthd s _) = False


validateTypeNames :: (M,TypeDirectory) -> Bool
validateTypeNames m = foldl (&&) True (map (validType (snd m)) (getTypeNamesUsed (fst m)))

validType :: TypeDirectory -> String -> Bool
validType dir s = if(s `elem` (map (\t -> getTypeName t) dir))then  True else error( "Invalid Type " ++s) 

getTypeNamesUsed :: M -> [String]
getTypeNamesUsed m = catMaybes (concatMap (\x -> getTypeChanges (snd x))  (snd m))

getTypeChanges :: Chng -> [Maybe String]
getTypeChanges (C s n) = if (isTypeChange s) then [Just n] else [Nothing]
getTypeChanges (E s n) =  [Nothing]
getTypeChanges (Seq c1 c2) = getTypeChanges c1 ++ getTypeChanges c2
