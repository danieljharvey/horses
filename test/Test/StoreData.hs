{-# LANGUAGE OverloadedStrings #-}

module Test.StoreData where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Syntax (parseExpr)
import Language.Mimsa.Types

fstExpr :: StoreExpression
fstExpr =
  unsafeGetExpr "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleFirst"

sndExpr :: StoreExpression
sndExpr = unsafeGetExpr "\\tuple -> let (tupleFirst,tupleSecond) = tuple in tupleSecond"

eqTenExpr :: StoreExpression
eqTenExpr =
  unsafeGetExpr "\\i -> eq(10)(i)"

compose :: StoreExpression
compose =
  unsafeGetExpr "\\f -> \\g -> \\aValue -> f(g(aValue))"

idExpr :: StoreExpression
idExpr = unsafeGetExpr "\\i -> i"

stdLib :: Project
stdLib = Project store' bindings' mempty
  where
    store' =
      Store $
        M.fromList
          [ (ExprHash 1, fstExpr),
            (ExprHash 3, eqTenExpr),
            (ExprHash 6, compose),
            (ExprHash 7, sndExpr),
            (ExprHash 11, idExpr)
          ]
    bindings' =
      VersionedBindings $
        M.fromList
          [ (mkName "fst", pure $ ExprHash 1),
            (mkName "eqTen", pure $ ExprHash 3),
            (mkName "compose", pure $ ExprHash 6),
            (mkName "snd", pure $ ExprHash 7),
            (mkName "id", pure $ ExprHash 11)
          ]

unsafeGetExpr' :: Text -> Bindings -> StoreExpression
unsafeGetExpr' input bindings' =
  case parseExpr input of
    Right expr' -> StoreExpression bindings' expr'
    a -> error $ "Error evaluating " <> T.unpack input <> ": " <> show a

unsafeGetExpr :: Text -> StoreExpression
unsafeGetExpr input = unsafeGetExpr' input mempty
