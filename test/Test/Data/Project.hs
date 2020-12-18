{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Project
  ( stdLib,
    idExpr,
  )
where

import Data.Functor
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Parser (parseExpr)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Utils.Helpers

type StoreExpressionA = StoreExpression Annotation

-- polymorphic export for use in other tests
idExpr :: (Monoid ann) => StoreExpression ann
idExpr = idExpr' $> mempty

fstExpr :: StoreExpressionA
fstExpr =
  unsafeGetExpr "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleFirst"

sndExpr :: StoreExpressionA
sndExpr = unsafeGetExpr "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleSecond"

eqTenExpr :: StoreExpressionA
eqTenExpr =
  unsafeGetExpr'
    "\\i -> eq(10)(i)"
    (Bindings $ M.singleton (mkName "eq") (exprHash 2))

compose :: StoreExpressionA
compose =
  unsafeGetExpr "\\f -> \\g -> \\aValue -> f(g(aValue))"

idExpr' :: StoreExpressionA
idExpr' = unsafeGetExpr "\\i -> i"

incrementInt :: StoreExpressionA
incrementInt =
  unsafeGetExpr'
    "\\a -> addInt(1)(a)"
    (Bindings $ M.singleton (mkName "addInt") (exprHash 18))

addInt :: StoreExpressionA
addInt = unsafeGetExpr "\\a -> \\b -> a + b"

eqExpr :: StoreExpressionA
eqExpr = unsafeGetExpr "\\a -> \\b -> a == b"

optionExpr :: StoreExpressionA
optionExpr = unsafeGetExpr "type Option a = Some a | Nowt in {}"

theseExpr :: StoreExpressionA
theseExpr = unsafeGetExpr "type These a b = This a | That b | These a b in {}"

aPairExpr :: StoreExpressionA
aPairExpr = unsafeGetExpr "(1,2)"

aRecordExpr :: StoreExpressionA
aRecordExpr = unsafeGetExpr "{ a: 1, b: \"dog\" }"

-- check removing annotation doesn't break stuff
stdLib :: Project Annotation
stdLib = (stdLib' $> ()) $> mempty

stdLib' :: Project Annotation
stdLib' = Project store' bindings' typeBindings'
  where
    store' =
      Store $
        M.fromList
          [ (exprHash 1, fstExpr),
            (exprHash 2, eqExpr),
            (exprHash 3, eqTenExpr),
            (exprHash 4, optionExpr),
            (exprHash 5, aPairExpr),
            (exprHash 6, compose),
            (exprHash 7, sndExpr),
            (exprHash 8, aRecordExpr),
            (exprHash 9, theseExpr),
            (exprHash 11, idExpr),
            (exprHash 17, incrementInt),
            (exprHash 18, addInt)
          ]
    bindings' =
      VersionedMap $
        M.fromList
          [ (mkName "fst", pure $ exprHash 1),
            (mkName "eq", pure $ exprHash 2),
            (mkName "eqTen", pure $ exprHash 3),
            (mkName "aPair", pure $ exprHash 5),
            (mkName "compose", pure $ exprHash 6),
            (mkName "snd", pure $ exprHash 7),
            (mkName "aRecord", pure $ exprHash 8),
            (mkName "id", pure $ exprHash 11),
            (mkName "incrementInt", pure $ exprHash 17),
            (mkName "addInt", pure $ exprHash 18)
          ]
    typeBindings' =
      VersionedMap $
        M.fromList
          [ (mkTyCon "Some", pure $ exprHash 4),
            (mkTyCon "Nowt", pure $ exprHash 4),
            (mkTyCon "This", pure $ exprHash 9),
            (mkTyCon "That", pure $ exprHash 9),
            (mkTyCon "These", pure $ exprHash 9)
          ]

unsafeGetExpr' :: Text -> Bindings -> StoreExpression Annotation
unsafeGetExpr' input bindings' =
  case parseExpr input of
    Right expr' -> StoreExpression expr' bindings' mempty
    a -> error $ "Error evaluating " <> T.unpack input <> ": " <> show a

unsafeGetExpr :: Text -> StoreExpression Annotation
unsafeGetExpr input = unsafeGetExpr' input mempty
