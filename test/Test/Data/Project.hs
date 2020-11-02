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
    (Bindings $ M.singleton (mkName "eq") (ExprHash 2))

compose :: StoreExpressionA
compose =
  unsafeGetExpr "\\f -> \\g -> \\aValue -> f(g(aValue))"

idExpr' :: StoreExpressionA
idExpr' = unsafeGetExpr "\\i -> i"

incrementInt :: StoreExpressionA
incrementInt =
  unsafeGetExpr'
    "\\a -> addInt(1)(a)"
    (Bindings $ M.singleton (mkName "addInt") (ExprHash 18))

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
stdLib' = Project store' bindings' typeBindings' mempty
  where
    store' =
      Store $
        M.fromList
          [ (ExprHash 1, fstExpr),
            (ExprHash 2, eqExpr),
            (ExprHash 3, eqTenExpr),
            (ExprHash 4, optionExpr),
            (ExprHash 5, aPairExpr),
            (ExprHash 6, compose),
            (ExprHash 7, sndExpr),
            (ExprHash 8, aRecordExpr),
            (ExprHash 9, theseExpr),
            (ExprHash 11, idExpr),
            (ExprHash 17, incrementInt),
            (ExprHash 18, addInt)
          ]
    bindings' =
      VersionedMap $
        M.fromList
          [ (mkName "fst", pure $ ExprHash 1),
            (mkName "eq", pure $ ExprHash 2),
            (mkName "eqTen", pure $ ExprHash 3),
            (mkName "aPair", pure $ ExprHash 5),
            (mkName "compose", pure $ ExprHash 6),
            (mkName "snd", pure $ ExprHash 7),
            (mkName "aRecord", pure $ ExprHash 8),
            (mkName "id", pure $ ExprHash 11),
            (mkName "incrementInt", pure $ ExprHash 17),
            (mkName "addInt", pure $ ExprHash 18)
          ]
    typeBindings' =
      VersionedMap $
        M.fromList
          [ (mkTyCon "Some", pure $ ExprHash 4),
            (mkTyCon "Nowt", pure $ ExprHash 4),
            (mkTyCon "This", pure $ ExprHash 9),
            (mkTyCon "That", pure $ ExprHash 9),
            (mkTyCon "These", pure $ ExprHash 9)
          ]

unsafeGetExpr' :: Text -> Bindings -> StoreExpression Annotation
unsafeGetExpr' input bindings' =
  case parseExpr input of
    Right expr' -> StoreExpression expr' bindings' mempty
    a -> error $ "Error evaluating " <> T.unpack input <> ": " <> show a

unsafeGetExpr :: Text -> StoreExpression Annotation
unsafeGetExpr input = unsafeGetExpr' input mempty
