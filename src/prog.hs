module Prog where

import Semantics
import AST
import Translator
import Matcher
import ASTChecks

int :: Type
int = Ty (TD "int" [])

void :: Type
void = Ty (TD "void" [])

getFoo :: Mthd
getFoo = M "getFoo" "foo" "int" []

setFoo :: Mthd
setFoo = M "setFoo" "foo" "void" ["int"]


getBoo :: Mthd
getBoo = M "getBoo" "boo" "int" []

setBoo :: Mthd
setBoo = M "setBoo" "boo" "void" ["int"]

foo :: Type
foo = Ty (TD "foo" [getFoo,setFoo])

boo :: Type
boo = Ty (TD "boo" [getBoo,setBoo])

dir :: TypeDirectory
dir  = [int,foo,boo]

fooToBooM :: M
fooToBooM = T ("foo","boo")  [(S (("foo",M_D "getFoo" "int" []),("boo", M_D "getBoo" "int" []))),
            (S (("foo",M_D "setFoo" "void" ["int"]),("boo", M_D "setBoo" "void" ["int"]))),
            (S (("foo",V_D "foo"),("boo", V_D "boo"))),
            (E (("foo",I_d "foo"),("boo", I_d "boo"))),
            (E (("int",M_I "foo" "getFoo" []),("int", M_I "boo" "getBoo" [])))]

-- Tests negative ----

fooToBooM1 :: M
fooToBooM1 = T ("foo","boo")  [(S (("foo",M_D "getFoo" "int" []),("boo", M_D "getBoo0" "int" []))),
            (S (("foo",M_D "setFoo" "void" ["int"]),("boo", M_D "setBoo" "void" ["int"]))),
            (S (("foo",V_D "foo"),("boo", V_D "boo"))),
            (E (("foo",I_d "foo"),("boo", I_d "boo"))),
            (E (("int",M_I "foo" "getFoo" []),("int", M_I "boo" "getBoo" [])))]
