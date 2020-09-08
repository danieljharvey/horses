{-# LANGUAGE OverloadedStrings #-}

module Test.StoreData where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Parser (parseExpr)
import Language.Mimsa.Types

fstExpr :: StoreExpression
fstExpr =
  unsafeGetExpr "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleFirst"

sndExpr :: StoreExpression
sndExpr = unsafeGetExpr "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleSecond"

eqTenExpr :: StoreExpression
eqTenExpr =
  unsafeGetExpr'
    "\\i -> eq(10)(i)"
    (Bindings $ M.singleton (mkName "eq") (ExprHash 2))

compose :: StoreExpression
compose =
  unsafeGetExpr "\\f -> \\g -> \\aValue -> f(g(aValue))"

idExpr :: StoreExpression
idExpr = unsafeGetExpr "\\i -> i"

incrementInt :: StoreExpression
incrementInt =
  unsafeGetExpr'
    "\\a -> addInt(1)(a)"
    (Bindings $ M.singleton (mkName "addInt") (ExprHash 18))

addInt :: StoreExpression
addInt = unsafeGetExpr "\\a -> \\b -> addIntPair((a,b))"

eqExpr :: StoreExpression
eqExpr = unsafeGetExpr "\\a -> \\b -> eqPair((a,b))"

stdLib :: Project
stdLib = Project store' bindings' mempty
  where
    store' =
      Store $
        M.fromList
          [ (ExprHash 1, fstExpr),
            (ExprHash 2, eqExpr),
            (ExprHash 3, eqTenExpr),
            (ExprHash 6, compose),
            (ExprHash 7, sndExpr),
            (ExprHash 11, idExpr),
            (ExprHash 17, incrementInt),
            (ExprHash 18, addInt)
          ]
    bindings' =
      VersionedBindings $
        M.fromList
          [ (mkName "fst", pure $ ExprHash 1),
            (mkName "eq", pure $ ExprHash 2),
            (mkName "eqTen", pure $ ExprHash 3),
            (mkName "compose", pure $ ExprHash 6),
            (mkName "snd", pure $ ExprHash 7),
            (mkName "id", pure $ ExprHash 11),
            (mkName "incrementInt", pure $ ExprHash 17),
            (mkName "addInt", pure $ ExprHash 18)
          ]

unsafeGetExpr' :: Text -> Bindings -> StoreExpression
unsafeGetExpr' input bindings' =
  case parseExpr input of
    Right expr' -> StoreExpression bindings' expr'
    a -> error $ "Error evaluating " <> T.unpack input <> ": " <> show a

unsafeGetExpr :: Text -> StoreExpression
unsafeGetExpr input = unsafeGetExpr' input mempty
