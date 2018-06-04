module NewExecute where

import NewAST
import NewSemantic
import NewTranslator
import Util
import NewInference
import NewErrors
import Data.List
import Data.Maybe
import SyntaxChecks


translate :: (M,TypeDirectory)-> (Msem,TypeDirectory)
translate m = let t = (getTypesInContext m)
              in ((t, map (mapTupleSep (translateToConstruct (fst t))(translateToChange)) (snd (fst m))),snd m)

applySeq :: C -> Construct -> Construct
applySeq (x:xs) c = applySeq xs ((fst x) c)
applySeq [] c = c

getChanges :: C -> [Chng]
getChanges xs = map (\x -> snd x) xs

applyToGen :: ((Msem,[Construct]),TypeDirectory) -> Maybe Res
applyToGen a = let resMapping = (map (\x -> srchNapply(fst (fst a),x)) ((snd.fst) a))
               in if (checkRes resMapping (snd a)) then Just ((fst.fst.fst) a,resMapping) else Nothing

srchNapply :: (Msem,Construct) -> ((Construct,Maybe Construct),[Chng])
srchNapply a =  maybe (((snd a,Nothing),[DO_NOT_MIGRATE])) (\x -> (clean.enrich) ((snd a,Just (applySeq (snd x) (snd a))),getChanges (snd x))) (find (\x -> fst x == snd a) (snd (fst a)))

execute :: (M,TypeDirectory) ->  ((TypeName,TypeName),ResMapping)
execute m = if (validateSyntax m )
                   then (prettyPrint.applyChngs2Gen) m  else error "FAILED!!!"

applyChngs2Gen :: (M, TypeDirectory) -> Maybe Res
applyChngs2Gen a = let g = generatePossibleConstructs ((fst.fst.fst) t)
                       t = (translate a)
                   in applyToGen ((fst t, g),snd a)

chngForOvrdnMthd :: Chng -> Maybe String
chngForOvrdnMthd (C (OveridenMthd n _ ) _ ) = Just n
chngForOvrdnMthd _ = Nothing

getChngdOvrdnMthd :: [Chng] -> [String]
getChngdOvrdnMthd a = catMaybes (map (\x -> chngForOvrdnMthd x) a)

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

enrich :: ((Construct,Maybe Construct),[Chng]) -> ((Construct,Maybe Construct),[Chng])
enrich x =  updtSnd  (++ (map (\x -> DONT_MIGRAtE x)
   (listA_minus_B (getChngdOvrdnMthd (snd x)) (getMthdsFromCls ((fst.fst) x))))) x


checkRes :: ResMapping -> TypeDirectory -> Bool
checkRes x dir = foldl (&&) True (map (validConstruct dir) (map (\y->(snd (fst y))) x))

getAllMethodsInDir :: TypeDirectory -> [Mthd]
getAllMethodsInDir dir = concat (map getMethods_Type dir)


validConstruct ::TypeDirectory ->  Maybe Construct  -> Bool
validConstruct dir (Just (CallSite m)) = if (elem m (getMethods_Type (getTypeFromName dir (fst m)))) then True else error (invalidCallsite m)
--validConstruct dir (Just (ClassDecl n m1)) = foldl (&&) True (map (\m -> if (elem m (getMethods_Type (getTypeFromName dir n))) then True else error ("MD"++ getMthdName m)) (mthdFrmMthdDecl m1))
validConstruct _   (Just _) = True
validConstruct _ Nothing     = True

removeMethodFrmCls :: (Construct -> Bool)  -> Construct -> Construct
removeMethodFrmCls f (ClassDecl n m ) = (ClassDecl n (removeElem f m) )
removeMethodFrmCls _ c = c

matchMethods :: [String] -> Construct -> Bool
matchMethods xs (MethodDeclaration m) = (getMthdName m)`elem` xs
matchMethods xs _ = False

getDontMigrateMthdName :: [Chng] -> [String]
getDontMigrateMthdName ((DONT_MIGRAtE s):ms) = (s:(getDontMigrateMthdName ms))
getDontMigrateMthdName (_:ms) = (getDontMigrateMthdName ms)
getDontMigrateMthdName [] = []

clean :: ((Construct,Maybe Construct),[Chng]) -> ((Construct,Maybe Construct),[Chng])
clean x = let i = (getMthdsFromCls ((fst.fst) x))
              j = (getChngdOvrdnMthd (snd x))
            in (updtFst.updtSnd) (\c -> (maybe Nothing (\y -> Just (removeMethodFrmCls (matchMethods (getDontMigrateMthdName (snd x))) y)) c)) x


getMthdDeclName  :: Construct -> Maybe String
getMthdDeclName (MethodDeclaration m) = Just (getMthdName m)
getMthdDeclName m                     = Nothing

