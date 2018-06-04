module NewErrors where

import NewSemantic

invalidCallsite :: Mthd -> String
invalidCallsite m =  "invalid Callsite generated for "++ (getMthdName m) ++
   show(m)
