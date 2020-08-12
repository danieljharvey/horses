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

isTenExpr :: StoreExpression
isTenExpr =
  unsafeGetExpr "\\i -> if eqInt(i)(10) then Right i else Left i"

eqTenExpr :: StoreExpression
eqTenExpr =
  unsafeGetExpr "\\i -> eqInt(10)(i)"

fmapSum :: StoreExpression
fmapSum =
  unsafeGetExpr
    "\\f -> \\a -> case a of Left (\\l -> Left l) | Right (\\r -> Right f(r))"

listUncons :: StoreExpression
listUncons =
  unsafeGetExpr "\\myList -> let [headA,tailA] = myList in (headA,tailA)"

listHead :: StoreExpression
listHead =
  unsafeGetExpr' "compose(fst)(listUncons)" $
    Bindings
      ( M.fromList
          [ (mkName "compose", ExprHash 6),
            (mkName "fst", ExprHash 1),
            (mkName "listUncons", ExprHash 5)
          ]
      )

listTail :: StoreExpression
listTail =
  unsafeGetExpr' "compose(snd)(listUncons)" $
    Bindings
      ( M.fromList
          [ (mkName "compose", ExprHash 6),
            (mkName "snd", ExprHash 7),
            (mkName "listUncons", ExprHash 5)
          ]
      )

compose :: StoreExpression
compose =
  unsafeGetExpr "\\f -> \\g -> \\aValue -> f(g(aValue))"

list :: StoreExpression
list = unsafeGetExpr' "{ head: listHd, tail: listTl }" bindings'
  where
    bindings' =
      Bindings
        ( M.fromList
            [ (mkName "listHd", ExprHash 8),
              (mkName "listTl", ExprHash 9)
            ]
        )

idExpr :: StoreExpression
idExpr = unsafeGetExpr "\\i -> i"

justExpr :: StoreExpression
justExpr = unsafeGetExpr "\\a -> Right a"

nothingExpr :: StoreExpression
nothingExpr = unsafeGetExpr "Left Unit"

maybeExpr :: StoreExpression
maybeExpr =
  unsafeGetExpr'
    "{ just: maybeJust, nothing: maybeNothing}"
    ( Bindings
        ( M.fromList
            [ (mkName "maybeJust", ExprHash 12),
              (mkName "maybeNothing", ExprHash 13)
            ]
        )
    )

listCons :: StoreExpression
listCons =
  unsafeGetExpr
    "\\a -> (\\maybeList -> case maybeList of Left (\\l -> [a]) | Right (\\as -> (appendList(as)([a]))))"

listFilter :: StoreExpression
listFilter =
  unsafeGetExpr'
    "\\pred -> \\as -> let foldFunc = \\a -> \\total -> if pred(a) then Right listCons(a)(total) else total in reduceList(foldFunc)(Left Unit)(as)"
    (Bindings $ M.singleton (mkName "listCons") (ExprHash 15))

stdLib :: Project
stdLib = Project store' bindings' mempty
  where
    store' =
      Store $
        M.fromList
          [ (ExprHash 1, fstExpr),
            (ExprHash 2, isTenExpr),
            (ExprHash 3, eqTenExpr),
            (ExprHash 4, fmapSum),
            (ExprHash 5, listUncons),
            (ExprHash 6, compose),
            (ExprHash 7, sndExpr),
            (ExprHash 8, listHead),
            (ExprHash 9, listTail),
            (ExprHash 10, list),
            (ExprHash 11, idExpr),
            (ExprHash 12, justExpr),
            (ExprHash 13, nothingExpr),
            (ExprHash 14, maybeExpr),
            (ExprHash 15, listCons),
            (ExprHash 16, listFilter)
          ]
    bindings' =
      VersionedBindings $
        M.fromList
          [ (mkName "fst", pure $ ExprHash 1),
            (mkName "isTen", pure $ ExprHash 2),
            (mkName "eqTen", pure $ ExprHash 3),
            (mkName "fmapSum", pure $ ExprHash 4),
            (mkName "listUncons", pure $ ExprHash 5),
            (mkName "compose", pure $ ExprHash 6),
            (mkName "snd", pure $ ExprHash 7),
            (mkName "listHead", pure $ ExprHash 8),
            (mkName "listTail", pure $ ExprHash 9),
            (mkName "list", pure $ ExprHash 10),
            (mkName "id", pure $ ExprHash 11),
            (mkName "maybe", pure $ ExprHash 14),
            (mkName "listCons", pure $ ExprHash 15),
            (mkName "listFilter", pure $ ExprHash 16)
          ]

unsafeGetExpr' :: Text -> Bindings -> StoreExpression
unsafeGetExpr' input bindings' =
  case parseExpr input of
    Right expr' -> StoreExpression bindings' expr'
    a -> error $ "Error evaluating " <> T.unpack input <> ": " <> show a

unsafeGetExpr :: Text -> StoreExpression
unsafeGetExpr input = unsafeGetExpr' input mempty
