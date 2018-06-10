module Prog where

import Semantic
import AST
import Translator
import Execute
--import Language.Java.Parser


int :: Type
int = Ty ("int", [])


bool :: Type
bool = Ty ("bool", [])

void :: Type
void = Ty ("void", [])

getFoo1 :: Mthd
getFoo1 = ("foo",("getFoo1", ("int", [])))


getFoo2 :: Mthd
getFoo2 = ("foo",("getFoo2", ("int", [])))

toStringFoo = ("foo",("toString", ("int", [])))
toStringBoo = ("boo",("toString", ("int", [])))

getFoo :: Mthd
getFoo = ("foo",("getFoo", ("foo", [])))

equalsFoo :: Mthd
equalsFoo = ("foo",("equals",("bool",[(0,"int")])))

equalsBoo :: Mthd
equalsBoo = ("boo",("equals",("bool",[(0,"int")])))

setFoo :: Mthd
setFoo =  ("foo",("setFoo", ("void",[(0,"int")])))

getBoo :: Mthd
getBoo = ( "boo",("getBoo",( "boo", [])))

getBoo1 :: Mthd
getBoo1 = ("boo",("getBoo1", ("bool", [])))

getBoo2 :: Mthd
getBoo2 = ("boo",("getBoo2", ("bool", [])))

setBoo :: Mthd
setBoo = ("boo", ("setBoo",("void",[(0,"int")])))

setBoo_bool :: Mthd
setBoo_bool = ("boo", ("setBoo",("void",[(0,"bool")])))

foo :: Type
foo = Ty ("foo", [getFoo,setFoo,getFoo1,equalsFoo,getFoo2,toStringFoo])

boo :: Type
boo = Ty ("boo", [getBoo,setBoo,equalsBoo,getBoo1,getBoo2,toStringBoo])

dir :: TypeDirectory
dir  = [int,foo,boo,bool]



--
fooToBooMClsMap =(("foo","boo"), [
                           (CLSDECL,
                           ((C (OveridenMthd "getFoo" ReturnType) "boo")
                           `Seq` (C  (OveridenMthd "getFoo" Name ) "getBoo")
                           `Seq`(C  (OveridenMthd "setFoo" Name ) "setBoo")
                           `Seq` (C SubType "boo")))
                           ])

fooToBooM3CallSiteMap =(("foo","boo"), [ (CALLSITE ["getFoo"], (C  Name "getBoo")),
                           (CALLSITE ["getFoo"], (C ReturnType "boo")),
                           (CALLSITE ["setFoo"], (C  Name "setBoo")),
                           (CALLSITE ["getFoo1"],(C Name "getBoo1")),
                           (CALLSITE ["getFoo2"],(C Name "getBoo2")),
                           (CALLSITE ["getFoo1","getFoo2"], (C ReturnType "bool"))])


intToBool = (("int","bool") , [])


fooToBooM3CallSiteMap1 =(("foo","boo"), [ (CALLSITE ["getFoo"], (C  Name "getBoo")),
                           (CALLSITE ["getFoo"], (C ReturnType "boo")),
                           (CALLSITE ["getFoo1","getFoo2"], (C ReturnType "bool")),
                           (CALLSITE ["getFoo","setFoo","getFoo1","getFoo2"], (E Name (\x -> replace x "Foo" "Boo" ) ))
                                       ])

callSiteNameMap = [("getFoo","getBoo"),("setFoo","setBoo"),("getFoo1","getBoo1"),("getFoo2","getBoo2")]

mapping :: [(String,String)] -> String -> String
mapping (x:xs) = \m ->  if (fst x == m) then  (snd x) else (mapping xs) m
mapping []  = error "Method not found"

fooToBooM3CallSiteMap2 =(("foo","boo"), [(CALLSITE ["getFoo"], (C ReturnType "boo")),
                           (CALLSITE ["getFoo1","getFoo2"], (C ReturnType "bool")),
                           (CALLSITE ["getFoo","setFoo","getFoo1","getFoo2"], (E Name (\x -> (mapping callSiteNameMap) x) ))
                                       ])

list = [fooToBooMClsMap,fooToBooM3CallSiteMap2,intToBool]

updtElem :: (a -> a) -> [a] -> [a]
updtElem f (x:xs) = (f x : updtElem f xs)
updtElem f []     = []


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)


--ast =  parser compilationUnit `fmap` readFile "Employee.java"

--ast1 =  parser methodDecl `fmap` readFile "Employee.java"
