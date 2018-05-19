module Constraints where

import TypeStructure

assignedTo :: Expression -> VariableDecl -> Bool
assignedTo  (Id (SubTy t1)) x@(VD (Ty ti2)) = assignedTo (Id t1) x
assignedTo  (Id i@(Ty ti1)) (VD j@ (Ty ti2)) = i == j
assignedTo  i@(Id (Ty ti1)) (VD j@(SubTy t2)) = assignedTo i ((VD t2))


