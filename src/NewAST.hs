module NewAST where

type TypeName = String
type Name = String
type LCTransform = (ConstructKind,Chng)

data Chng = Seq Chng Chng
         | For SubConstruct Chng
         | C SubConstruct String
         | DO_NOT_MIGRATE
         | DONT_MIGRAtE String
        deriving (Show)

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

data ConstructKind = CLSDECL | ALLOCATION | CALLSITE String  | NEW_CLASS
                   deriving (Show)


isTypeChange :: SubConstruct -> Bool
isTypeChange Name       = False
isTypeChange (OveridenMthd s c)  = isTypeChange c
isTypeChange _          = True


