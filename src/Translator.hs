module Translator where

import AST
import Semantics
import ASTChecks

translateStmt :: (String,Stmt) -> (TypeName,Statement)
translateStmt (tn,M_D mn rt ps)  = (tn, (MD (M  mn tn rt ps)))
translateStmt (tn,V_D t)         = (tn,VD t)

translateExpr :: (String, Expr)  -> (TypeName,Expression)
translateExpr (tn, I_d t) = (tn,Id t)
translateExpr (rt, M_I t mn ps) = (rt, (MI t (M mn t rt ps)))

translateMapping :: Mapping -> MappingSem
translateMapping (S (m1,m2))  = SSem ((translateStmt m1),((translateStmt m2)))
translateMapping (E (m1,m2))  = ESem ((translateExpr m1),(translateExpr m2))

translate :: M  -> Msem
translate m@(T (u,v) ms)  = (Tsem (u,v) (map translateMapping ms))
translate m@(Seq s1 s2)   = translate s1 -- ++ translate s2
