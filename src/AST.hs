module AST where

type TypeName = String
type Name = String
type LCTransform = (ConstructKind,Chng)

data Chng = Seq Chng Chng
         | E SubConstruct (String -> String)
         | C SubConstruct String
         | DO_NOT_MIGRATE
         | DONT_MIGRAtE String
         | IDENTITY
--        deriving (Show)

instance Show Chng where
  show (Seq c1 c2) = show c1 ++ "; " ++ show c2
  show (E s f) = "Edit: " ++ show s ++ " Some pattern"
  show (C s t) = "Change: " ++ show s ++ " " ++ t
  show (DONT_MIGRAtE m) = "DO NOT MIGRATE " ++ m
  show (DO_NOT_MIGRATE) = "DO NOT MIGRATE "
  show (IDENTITY)       = "IDENTITY"

data SubConstruct = ReturnType
                   | Param Int
                   | Arg Int
                   | To
                   | Receiver
                   | MethodInvc
                   | OveridenMthd String SubConstruct
                   | SubType
                   | Name
                   deriving (Show)



type M = ((TypeName,TypeName),[LCTransform])

data ConstructKind = CLSDECL | ALLOCATION | CALLSITE [String]  | NEW_CLASS
                   deriving (Show)


isTypeChange :: SubConstruct -> Bool
isTypeChange Name       = False
isTypeChange (OveridenMthd s c)  = isTypeChange c
isTypeChange _          = True


