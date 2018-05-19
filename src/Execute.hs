module Execute where

import Matcher
import Semantics
import Prog
import AST
import ASTChecks
import StateMonad
import Translator
import SemanticChecks

translateToSemWithChecks :: M -> TypeDirectory -> Either Msem [String]
translateToSemWithChecks m dir = let msem = (translate m)
                  in
                   case (cleanErrors ((astChecks m) ++ (semChecks msem dir))) of []-> Left msem
                                                                                 x:xs -> Right (x:xs)

execute :: M -> TypeDirectory -> Either [Change] [String]
execute m dir = let i = translateToSemWithChecks m dir
                in case (i) of Left m -> Left (cleanChanges (getChanges i))
                               Right s -> Right s



cleanChanges :: [Maybe Change] -> [Change]
cleanChanges [] = []
cleanChanges ((Just x) : xs) = x : cleanChanges xs
cleanChanges ((Nothing) : xs) = cleanChanges xs

cleanErrors :: [String] -> [String]
cleanErrors [] = []
cleanErrors (x:xs) = if (isNotBlank x) then (x:cleanErrors xs) else cleanErrors xs


