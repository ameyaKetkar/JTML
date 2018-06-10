module Execute where

import AST
import Semantic
import Translator
import Util
import Errors
import Data.List
import Data.Maybe
import SyntaxChecks
import ManageChange
import SemanticFunctions
import ResultChecks


-- Validate Syntax  -- Checks if type names are valid, and correct subconstruct is used
--  t = translate Input
--  g = generate Possible Constructs
--   apply changes (t,g)
--    enrich : Infer Do not Migrate for unmapped constructs
--    clean  : removes unmapped method declarations in CLS
--    Check result mapping
--       (ValidateConstruct) -> Checks if method invocation and class contains correct Mthd


preProcessInput :: [M] -> [M]
preProcessInput ms = map (\g -> (getTypeMapping g,(collectChanges g))) (groupBy (groupChangesPred) ms)

groupChangesPred :: M -> M -> Bool
groupChangesPred a b = fst a == fst b

getTypeMapping :: [M] -> (TypeName,TypeName)
getTypeMapping (m:ms) = fst m
getTypeMapping []     = error "No Type Mapping?"

collectChanges :: [M] -> [LCTransform]
collectChanges ms = concatMap (\m -> snd m) ms

execute :: ([M],TypeDirectory) ->  String
execute (ms,dir) = concat (map (\m ->
                       if (validateSyntax (m,dir))
                       then (prettyPrint.applyChngs2Gen) (m,dir)  else error "FAILED!!!")  (preProcessInput ms))

applyChngs2Gen :: (M, TypeDirectory) -> Maybe Res
applyChngs2Gen a = let g = generatePossibleConstructs ((fst.fst.fst) t)
                       t = (translate a)
                       r = (defaultChanges.applyToGen) ((fst t, g),snd a)
                    in if (checkRes (snd r) ( snd a)) then  Just r else Nothing

defaultChanges ::  Res -> Res
defaultChanges  r =  (manageReceiver (appendAllocation r))

migrateReceiver :: (TypeName,TypeName) -> ((Construct,Maybe Construct),[Chng]) -> ((Construct,Maybe Construct),[Chng])
migrateReceiver t@(t1,t2) c@(((CallSite m1),(Just (CallSite m2))),cs) = mapTupleSep (updtSnd (maybe Nothing (\x ->(Just (CallSite(updtFst (\y -> t2) m2))) ))) (++ [C Receiver t2]) c
migrateReceiver m n@(_,_) = n

manageReceiver :: Res -> Res
manageReceiver (t,r) = (t,map (migrateReceiver (mapTuple (getTypeName) t)) r)

appendAllocation :: Res -> Res
appendAllocation r = updtSnd (++ [((Allocation (getTypeName ((fst.fst) r)),Just (Allocation ( getTypeName ((snd.fst) r)))),[C To (getTypeName((snd.fst) r))])]) r

translateToConstructChange :: Type -> (ConstructKind,Chng) ->[(Construct,C)]
translateToConstructChange t c = let con = translateToConstruct t (fst c)
                                     ch  = translateToChange (snd c)
                                 in map (\x -> (x,ch)) con

translate :: (M,TypeDirectory)-> (Msem,TypeDirectory)
translate m = let t = (getTypesInContext m)
              in ((t, concatMap (translateToConstructChange (fst t)) (snd (fst m))),snd m)

applyToGen :: ((Msem,[Construct]),TypeDirectory) ->  Res
applyToGen a = let resMapping = (map (\x -> srchNapply(fst (fst a),x)) ((snd.fst) a))
                in (((fst.fst.fst) a),resMapping)



srchNapply :: (Msem,Construct) -> ((Construct,Maybe Construct),[Chng])
srchNapply a = let matches = concatMap (snd) (filter (\x -> fst x == snd a) (snd (fst a)))
               in case (matches) of [] ->   smartMapMthds ((fst.fst) a) (snd a)
                                    xs ->   (clean.enrich) ((snd a,Just (applySeq matches (snd a))),getChanges (matches))

smartMapMthds :: (Type,Type) -> Construct -> ((Construct,Maybe Construct),[Chng])
smartMapMthds (t1,t2) c@(CallSite m) = let i = (updtFst (\t->getTypeName t2) m)
                                       in if (i `elem` (getMethods_Type t2)) then ((c,Just (CallSite i)),[IDENTITY]) else ((c,Nothing),[DO_NOT_MIGRATE])
smartMapMthds (t1,t2) c           =   ((c,Nothing),[DO_NOT_MIGRATE])

prettyPrint :: Maybe Res ->String
prettyPrint = maybe (error " Could not print the result") printRes

printRes :: Res -> String
printRes r = show (mapTuple getTypeName (fst r)) ++ "\n" ++ concat (map printResMapping (snd r))

printResMapping :: ((Construct,Maybe Construct),[Chng]) -> String
printResMapping m = printConstruct ((fst.fst) m) ++ " ... " ++
 ( maybe "X" (\x -> printConstruct x)((snd.fst) m)) ++ " ... " ++ show (snd m) ++ "\n"

-- If mthd decl in cls are not mapped then addes DONT_MIGRATE
enrich :: ((Construct,Maybe Construct),[Chng]) -> ((Construct,Maybe Construct),[Chng])
enrich x =  updtSnd  (++ (map (\x -> DONT_MIGRAtE x)
   (listA_minus_B (getChngdOvrdnMthd (snd x)) (getMthdsFromCls ((fst.fst) x))))) x

-- removes unmapped method declarations in CLS
clean :: ((Construct,Maybe Construct),[Chng]) -> ((Construct,Maybe Construct),[Chng])
clean x = let i = (getMthdsFromCls ((fst.fst) x))
              j = (getChngdOvrdnMthd (snd x))
            in (updtFst.updtSnd) (\c -> (maybe Nothing (\y -> Just (removeMethodFrmCls (matchMethods (getDontMigrateMthdName (snd x))) y)) c)) x
