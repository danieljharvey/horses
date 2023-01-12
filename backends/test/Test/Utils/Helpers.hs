module Test.Utils.Helpers (int, bool, unknown, str', str, mtBool, mtString, mtVar, mtInt, tvNamed, tvNum, typeName, mtFun) where

import Data.Text (Text)
import Language.Mimsa.Core

bool :: (Monoid ann) => Bool -> Expr a ann
bool a = MyLiteral mempty (MyBool a)

int :: (Monoid ann) => Int -> Expr a ann
int a = MyLiteral mempty (MyInt a)

str :: (Monoid ann) => StringType -> Expr a ann
str a = MyLiteral mempty (MyString a)

str' :: (Monoid ann) => Text -> Expr a ann
str' = str . StringType

--
unknown :: (Monoid ann) => Int -> Type ann
unknown = MTVar mempty . TVUnificationVar

typeName :: (Monoid ann) => Text -> Type ann
typeName = MTVar mempty . TVName . mkTyVar

---

tvNum :: Int -> TypeIdentifier
tvNum = TVUnificationVar

tvNamed :: Text -> TypeIdentifier
tvNamed t = TVName $ mkTyVar t

----

mtInt :: (Monoid ann) => Type ann
mtInt = MTPrim mempty MTInt

mtBool :: (Monoid ann) => Type ann
mtBool = MTPrim mempty MTBool

mtString :: (Monoid ann) => Type ann
mtString = MTPrim mempty MTString

mtVar :: (Monoid ann) => Text -> Type ann
mtVar n = MTVar mempty (tvNamed n)

mtFun :: (Monoid ann) => Type ann -> Type ann -> Type ann
mtFun = MTFunction mempty

----
