module Test.Utils.Helpers where

import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Parser
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

fromRight :: (Printer e) => Either e a -> a
fromRight either' = case either' of
  Left e -> error (T.unpack $ prettyPrint e)
  Right a -> a

unsafeParseExpr :: Text -> Expr Name ()
unsafeParseExpr t = case parseExpr t of
  Right a -> a $> ()
  Left _ ->
    error $
      "Error parsing expr for Prettier tests:"
        <> T.unpack t

getHashOfName :: Project ann -> Name -> ExprHash
getHashOfName prj name =
  case lookupBindingName prj name of
    Just a -> a
    _ -> error "could not getHashOfName"

createTestOrExplode ::
  Project Annotation ->
  StoreExpression Annotation ->
  TestName ->
  UnitTest
createTestOrExplode prj sExpr name = case createUnitTest prj sExpr name of
  Right a -> a
  _ -> error "EXPLODE"

getStoreExpression :: Project ann -> ExprHash -> StoreExpression ann
getStoreExpression project exprHash' =
  case lookupExprHash project exprHash' of
    Just a -> a
    _ -> error "could not getStoreExpression"

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
