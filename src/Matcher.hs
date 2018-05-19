module Matcher where

import AST
import Semantics

getChanges :: Either Msem [String] -> [Maybe Change]
getChanges (Left (Tsem _ m)) = map getCN m ++ (getCTs m)
getChanges (Left (Seqsem s1 s2)) =(getChanges (Left s1)) ++ (getChanges (Left s2))
getChanges (Right m) = []

-------------
getCN :: MappingSem-> Maybe Change
getCN (SSem ((x1,(MD (M mn _ _ _))),(x2,(MD (M mn1 _ _ _))))) =  if (mn == mn1) then Nothing else Just (CN mn mn1 MTHD_DECL)
getCN (SSem ((x1,_),(x2,_)))                                  = Nothing
getCN (ESem ((x1,(MI _ (M m _ _ _))),(x2, (MI _ (M mc _ _ _ ))))) =  if (mc == m) then Nothing else Just (CN m mc MTHD_INVC)
getCN (ESem ((x1,_),(x2,_)))                                  = Nothing
-------------

getChangeType :: (TypeName,TypeName) -> (LangConstruct -> Maybe Change)
getChangeType (t1,t2) = case (t1 == t2) of False -> \lc -> (Just (CT t1 t2 lc))
                                           True  -> \lc -> Nothing

getCTMthd :: (Mthd,Mthd) -> [Maybe Change]
getCTMthd ((M mn1 t1 rt1 xs),(M mn2 t2 rt2 ys)) = ((getChangeType (rt1,rt2)) MTHD_DECL) : (map ($ VRBL_DECL) (map  getChangeType (zip xs ys)))

-----------

getCTs :: [MappingSem] -> [Maybe Change]
getCTs (x:xs) = getCT x ++ getCTs xs
getCTs []     = []

getCT :: MappingSem -> [Maybe Change]
getCT (SSem ((x1,(MD m1)),(x2,(MD m2)))) = (getCTMthd (m1,m2))
getCT (SSem ((x1,(VD t1)),(x2,(VD t2)))) = [getChangeType (t1,t2) VRBL_DECL]
getCT (ESem _) = []


