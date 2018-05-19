module AST where

data Stmt = V_D String
         | M_D String String [String]
         deriving (Show)


data Expr = I_d String
           |M_I String String [String]
           deriving (Show)

data M = T (String,String) [Mapping]
         |Seq M M
         deriving (Show)

data Mapping =S ((String, Stmt),(String,Stmt))
              |E ((String, Expr),(String,Expr))
              deriving (Show)
