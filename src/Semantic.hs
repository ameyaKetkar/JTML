module Semantic where

import AST
import Util
import Data.List

type TypeInfo = (TypeName,[Mthd])

data Type = Ty TypeInfo
           |SubTy Type
          deriving (Show)

instance Eq Type where
  t1 == t2 =(getTypeName t1) == (getTypeName t2)

type Mthd = (TypeName,(Name,(TypeName, [(Int,TypeName)])))

showMethod :: Mthd -> String
showMethod m =  (fst m)++"(" ++(fst.snd) m ++ "(" ++(fst.snd.snd) m ++ "("++(intercalate "," (map (\p -> snd p) ((snd.snd.snd) m))) ++ ")"++")" ++")\n" 


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


printConstruct ::  Construct -> String
printConstruct  (CallSite m) = "Call Site : " ++ showMethod m
printConstruct  (Allocation m) = "Allocation : " ++ m ++ "\n"
printConstruct  (MethodDeclaration m) = "    Method Declaration : " ++ showMethod m
printConstruct  (ClassDecl t ms)     = "Class Declaration :" ++ t ++ "\n"++ (concat (map (\m -> printConstruct m) ms))



type TypeDirectory = [Type]

type C = [(Construct -> Construct,Chng)]

type Msem = ((Type,Type),[(Construct ,C)])

type ResMapping = [((Construct,Maybe Construct),[Chng])]

type Res =( (Type,Type),ResMapping)

getTypeName :: Type -> String
getTypeName (Ty t) = fst t
getTypeName (SubTy t)      = getTypeName t

getMthdDeclName1 (MethodDeclaration m) = getMthdName m

getMthdName = \x -> fst (snd x)

getMthdRetType :: Mthd -> TypeName
getMthdRetType (t1,(n,(t2,x))) =  t2
