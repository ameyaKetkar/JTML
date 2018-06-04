module NewProg where

import NewSemantic
import NewAST
import NewTranslator
import NewExecute
import NewInference

int :: Type
int = Ty ("int", [])


bool :: Type
bool = Ty ("bool", [])

void :: Type
void = Ty ("void", [])

getFoo1 :: Mthd
getFoo1 = ("foo",("getFoo1", ("int", [])))

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

setBoo :: Mthd
setBoo = ("boo", ("setBoo",("void",[(0,"int")])))

setBoo_bool :: Mthd
setBoo_bool = ("boo", ("setBoo",("void",[(0,"bool")])))

foo :: Type
foo = Ty ("foo", [getFoo,setFoo,getFoo1,equalsFoo])

boo :: Type
boo = Ty ("boo", [getBoo,setBoo,equalsBoo])

dir :: TypeDirectory
dir  = [int,foo,boo,bool]


fooToBooM :: M
fooToBooM =(("foo","boo"), [(ALLOCATION,C To "boo" ),
                            (CALLSITE "getFoo", (Seq (C Name "getBoo") (C Receiver "boo")))
                           ])

fooToBooM1 :: M
fooToBooM1 =(("foo","boo"), [(ALLOCATION,C To "boo" ),
                            (CALLSITE "getFoo", (C  Name "getBoo"))
                           ])

--
fooToBooM2 :: M
fooToBooM2 =(("foo","boo"), [(ALLOCATION,C To "boo" ),
                           (CALLSITE "getFoo", ((C  Name "getBoo") `Seq` (C Receiver "boo"))),
                           (CALLSITE "setFoo", ((C  Name "setBoo") `Seq` (C Receiver "boo"))),
                           (CLSDECL, ( --(C (OveridenMthd "getFoo" ReturnType) "boo")  `Seq`
                           (C  (OveridenMthd "getFoo" Name ) "getBoo") `Seq`
                           (C SubType "boo")))
                           ])

--
fooToBooM3 :: M
fooToBooM3 =(("foo","boo"), [
                           (ALLOCATION,C To "boo" ),
                           (CALLSITE "getFoo", ((C  Name "getBoo") `Seq` (C Receiver "boo")) `Seq` (C ReturnType "boo") )
                           ,
                           (CALLSITE "setFoo", ((C  Name "setBoo") `Seq` (C Receiver "boo")
                           --                     `Seq` (C (Arg 0) "bool")
                                               )),
                           (CLSDECL,
                           ((C (OveridenMthd "getFoo" ReturnType) "boo") `Seq`
                            (C  (OveridenMthd "getFoo" Name ) "getBoo")
                         --  `Seq` (C (OveridenMthd "setFoo" (Param 0)) "bool")
                           `Seq`(C  (OveridenMthd "setFoo" Name ) "setBoo")
                           `Seq` (C SubType "boo")))
                           ])

updtElem :: (a -> a) -> [a] -> [a]
updtElem f (x:xs) = (f x : updtElem f xs)
updtElem f []     = []



