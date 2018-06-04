module NewSemantic where

import NewAST
import Util
import Text.XML.HXT.Core
--- Type -----
type TypeInfo = (TypeName,[Mthd])

data Type = Ty TypeInfo
           |SubTy Type
          deriving (Show)
instance Eq Type where
  t1 == t2 =(getTypeName t1) == (getTypeName t2)

data MethodDecl = MD Mthd
              deriving (Show)

instance Eq MethodDecl where
  (MD m1) == (MD m2) = m1 == m2

data Construct = ClassDecl TypeName [Construct]
                | Allocation TypeName
                | CallSite Mthd
                | MethodDeclaration Mthd
                deriving (Show)

instance Eq Construct where
  (ClassDecl t1 m1) == (ClassDecl t2 m2) = (t1==t2) && (m1 == m2)
  (Allocation t1)   == (Allocation t2)   = (t1 == t2)
  (CallSite m1)     == (CallSite m2)     = (m1 == m2)
  (MethodDeclaration m1) == (MethodDeclaration m2) = m1 == m2
  _                 ==     _              = False

getTypeName :: Type -> String
getTypeName (Ty t) = fst t
getTypeName (SubTy t)      = getTypeName t

type TypeDirectory = [Type]

getTypeFromName :: TypeDirectory -> TypeName -> Type
getTypeFromName (x:xs) t= case ((getTypeName x) == t) of True -> x
                                                         False-> getTypeFromName xs t
getTypeFromName [] t = error "Type Not found in directory "

getMethods_Type :: Type -> [Mthd]
getMethods_Type (Ty m) = snd m
getMethods_Type (SubTy t) = getMethods_Type t


------ Constructs -----

type Mthd = (TypeName,(Name,(TypeName, [(Int,TypeName)])))

getMthdName = \x -> fst (snd x)

getMthdRetType :: Mthd -> TypeName
getMthdRetType (t1,(n,(t2,x))) =  t2

updtRetType :: Mthd -> TypeName -> Mthd
updtRetType (t1,(n,(t2,x))) t =   (t1,(n,(t,x)))

getMthdWithName :: [Mthd] -> Name -> Maybe Mthd
getMthdWithName [] n = Nothing
getMthdWithName (m:ms) n = if(getMthdName m == n) then Just m else getMthdWithName ms n


data Change  = ChangeName (Construct -> Construct)
             | ChangeType Construct TypeName (Construct -> SubConstruct -> TypeName)

type C = [(Construct -> Construct,Chng)]

type Msem = ((Type,Type),[(Construct ,C)])



getClassDecl t = [ClassDecl (getTypeName t) (map (\m ->MethodDeclaration m) (getMethods_Type t))]
getAllocations t = [Allocation (getTypeName t)]
getMethods t =  map (\x -> CallSite x) (getMethods_Type t)
