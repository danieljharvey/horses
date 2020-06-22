module Test.Helpers where

import Language.Mimsa.Types

bool :: Bool -> Expr
bool a = MyLiteral (MyBool a)

int :: Int -> Expr
int a = MyLiteral (MyInt a)

str :: StringType -> Expr
str a = MyLiteral (MyString a)

--
--
unknown :: Int -> MonoType
unknown i = MTUnknown (UniVar i)
