module NewInference where
import NewTranslator
import NewAST
import NewSemantic
import Util

inferredInfo x  = let r = inferMapping (getUnMappedConstructs x (fst(fst x))) (snd (fst x))
                      in  (fst x, r++(snd x))

getMappedConstructs r = map (\x -> (fst (fst x))) (snd r)



generatePossibleConstructs :: Type -> [Construct]
generatePossibleConstructs t = getAllocations t ++ getClassDecl t ++ getMethods t 

--propagateT2R :: (TypeName,TypeName) -> [((Construct,Maybe Construct),[Chng])] -> Construct
--propagateT2R (t1,t2) a@(Allocation t) = if (t1 == t) then (Allocation t2) else a
--propagateT2R (t1,t2) a@(CallSite  m) = if (t1 == (getMthdRetType m)) then (CallSite (updtRetType m t2)) else a
--propagateT2R (t1,t2) a@(MethodDeclaration m) = if (t1 ==  (getMthdRetType m)) then (MethodDeclaration (updtRetType m t2)) else a
--propagateT2R (t1,t2) a@(ClassDecl n m) = (ClassDecl n (map (propagateT2R (t1,t2)) m))
 



getUnMappedConstructs x y = listA_minus_B (getMappedConstructs x) (generatePossibleConstructs y)


inferMapping :: [Construct] -> Type ->  ResMapping
inferMapping xs t= map (\y -> case (inferMethod y t) of Nothing -> ((y,Nothing),[DO_NOT_MIGRATE])
                                                        Just x  -> ((y,Just x),[C Receiver (getTypeName t)])) xs


inferMethod :: Construct -> Type -> Maybe Construct
inferMethod (CallSite m) t = let i = (getTypeName t,(snd m))
                             in if (elem i (getMethods_Type t)) then Just (CallSite i) else Nothing
inferMethod _ t = Nothing

prettyPrint :: Maybe Res -> ((TypeName,TypeName),ResMapping)
prettyPrint (Just r) = ((mapTuple getTypeName (fst r)),snd r)
prettyPrint Nothing = error "Could not generate mapping"

pprettyPrint :: Maybe Res -> ((TypeName,TypeName),ResMapping)
pprettyPrint r = maybe (error " blah") (\x -> ((mapTuple getTypeName (fst x)),snd x)) r


