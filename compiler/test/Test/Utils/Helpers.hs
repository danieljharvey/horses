{-# LANGUAGE OverloadedStrings #-}

module Test.Utils.Helpers where

import Data.Bifunctor (first)
import Data.Functor
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Parser
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Tests.UnitTest
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Tests
import Language.Mimsa.Types.Typechecker

joinLines :: [Text] -> Text
joinLines = T.intercalate "\n"

fromRight :: (Printer e) => Either e a -> a
fromRight either' = case either' of
  Left e -> error (T.unpack $ prettyPrint e)
  Right a -> a

fromLeft :: Either e a -> e
fromLeft either' = case either' of
  Left e -> e
  Right _ -> error "Expected a Left!"

fromJust :: Maybe a -> a
fromJust maybe' = case maybe' of
  Just a -> a
  _ -> error "Expected a Just"

unsafeParseExpr' :: Monoid ann => Text -> Expr Name ann
unsafeParseExpr' t = case parseExpr t of
  Right a -> a $> mempty
  Left _ ->
    error $
      "Error parsing expr for Prettier tests:"
        <> T.unpack t

unsafeParseExpr :: Text -> Expr Name ()
unsafeParseExpr = unsafeParseExpr'

unsafeParseModuleItem :: (Monoid ann) => Text -> ModuleItem ann
unsafeParseModuleItem t = case parseAndFormat moduleParser t of
  Right [item] -> item $> mempty
  Right _many -> error "ModuleItem parser succeeded but did not have 1 item"
  Left e -> error $ "Error parsing ModuleItem for tests: " <> T.unpack (prettyPrint e)

unsafeParseMonoType :: Text -> Type ()
unsafeParseMonoType t = case parseMonoType t of
  Right a -> a $> ()
  Left _ ->
    error $
      "Error parsing monotype for Prettier tests:"
        <> T.unpack t

textErrorContains :: Text -> Either Text a -> Bool
textErrorContains s res = case res of
  Left e -> s `T.isInfixOf` e
  _ -> False

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
unknown = MTVar mempty . TVUnificationVar

typeName :: (Monoid ann) => Text -> Type ann
typeName = MTVar mempty . TVName . mkTyVar

---

tvNum :: Int -> TypeIdentifier
tvNum = TVUnificationVar

tvNamed :: Text -> TypeIdentifier
tvNamed t = TVName $ mkTyVar t

exprHash :: Int -> ExprHash
exprHash = ExprHash . T.pack . show

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

additionalTests :: Project ann -> Project ann -> Int
additionalTests old new =
  unitTestsSize new - unitTestsSize old
  where
    unitTestsSize :: Project ann -> Int
    unitTestsSize = M.size . prjTests

additionalStoreItems :: Project ann -> Project ann -> Int
additionalStoreItems old new =
  projectStoreSize new - projectStoreSize old
  where
    projectStoreSize :: Project ann -> Int
    projectStoreSize = length . getStore . prjStore

----------

evaluateText ::
  Project Annotation ->
  Text ->
  Either (Error Annotation) (ResolvedExpression Annotation)
evaluateText project input = do
  expr <- first (ParseError input) (parseExpr input)
  (_, _, re) <-
    Actions.run
      project
      (Actions.typecheckExpression project input expr)
  pure re
