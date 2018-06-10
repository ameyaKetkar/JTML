module Errors where

import Semantic

invalidCallsite :: Mthd -> String
invalidCallsite m =  "invalid Callsite generated for "++ (getMthdName m) ++
   show(m)
