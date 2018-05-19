module Semantics where

data TypeInfo = TD TypeName [Mthd]
               deriving (Show)

instance Eq Type where
  t1 == t2 =(getTypeName t1) == (getTypeName t2)

instance Eq Mthd where
  (M mn1 c1 rt1 xs1) == (M mn2 c2 rt2 xs2) = (mn1 == mn2) && (c1 == c2)
                                          && (rt1 == rt2) && (xs1==xs2)

type TypeName = String

data Type = Ty TypeInfo
           |SubTy Type
          deriving (Show)

type Vrbl = TypeName

data Mthd = M String TypeName TypeName [TypeName]
          deriving (Show)
-- name class rt pt

data Statement = VD Vrbl
              | MD Mthd
          deriving (Show)

data Expression = Id Vrbl
                 | MI TypeName Mthd
         deriving (Show)

data Msem = Tsem (TypeName,TypeName) [MappingSem]
         |Seqsem Msem Msem
         deriving (Show)

data MappingSem =SSem ((TypeName, Statement),(TypeName,Statement))
              |ESem ((TypeName, Expression),(TypeName,Expression))
             deriving (Show)

data LangConstruct = VRBL_DECL|MTHD_DECL|IDENTIFIER|MTHD_INVC
                    deriving (Show)

data Change = CN String String LangConstruct
              |CT TypeName TypeName LangConstruct
              |NoChange
              deriving (Show)

getTypeName :: Type -> String
getTypeName (Ty (TD n _)) = n
getTypeName (SubTy t)      = getTypeName t

type TypeDirectory = [Type]

getTypeFromName :: TypeName -> TypeDirectory -> Type
getTypeFromName t (x:xs) = case ((getTypeName x) == t) of True -> x
                                                          False-> getTypeFromName t xs
getTypeFromName t [] = error "Type Not found in directory "


getMthdName :: Mthd -> String
getMthdName (M n _ _ _) = n

data MthdSign = MS String String [String]

getMthdSign :: Mthd -> MthdSign
getMthdSign (M n _ rt p) = MS n rt p


