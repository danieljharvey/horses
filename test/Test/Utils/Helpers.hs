module Test.Utils.Helpers where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

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
unknown = MTVar mempty . TVNum

typeName :: (Monoid ann) => Text -> Type ann
typeName = MTVar mempty . TVName . mkTyVar

---

named :: Text -> Variable
named = NamedVar . Name

numbered :: Int -> Variable
numbered = NumberedVar

---

tvFree :: Int -> TypeIdentifier
tvFree = TVNum

tvNumbered :: Int -> TypeIdentifier
tvNumbered = TVNum

tvNamed :: Text -> TypeIdentifier
tvNamed t = TVName $ mkTyVar t

exprHash :: Int -> ExprHash
exprHash = ExprHash . T.pack . show
