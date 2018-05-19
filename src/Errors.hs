module Errors where
import Data.List

noBlanks :: String
noBlanks = "No blank spaces allowed"

mthdNotFound :: String -> String
mthdNotFound m  = "Method " ++ m ++ " Not found "

paramArgNotEqual :: String
paramArgNotEqual = "number of Parameter or Arguments to MD and MI are unequal "


