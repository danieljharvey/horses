{-# LANGUAGE OverloadedStrings #-}

module Test.StoreData where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Syntax (parseExpr)
import Language.Mimsa.Types

fstExpr :: StoreExpression
fstExpr =
  unsafeGetExpr "\\tuple -> let (a,b) = tuple in a"

sndExpr :: StoreExpression
sndExpr = unsafeGetExpr "\\tuple -> let (a,b) = tuple in b"

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
  unsafeGetExpr "\\myList -> let [head,tail] = myList in (head,tail)"

listHead :: StoreExpression
listHead =
  unsafeGetExpr' "\\i -> compose(fst)(listUncons)(i)" $ -- why does this work but not compose(fst)(listUncons)
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
  unsafeGetExpr "\\f -> \\g -> \\a -> f(g(a))"

list :: StoreExpression
list = unsafeGetExpr' "{ head: listHead, tail: listTail }" bindings'
  where
    bindings' =
      Bindings
        ( M.fromList
            [ (mkName "listHead", ExprHash 8),
              (mkName "listTail", ExprHash 9)
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

stdLib :: StoreEnv
stdLib = StoreEnv store' bindings'
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
            (ExprHash 14, maybeExpr)
          ]
    bindings' =
      Bindings $
        M.fromList
          [ (mkName "fst", ExprHash 1),
            (mkName "isTen", ExprHash 2),
            (mkName "eqTen", ExprHash 3),
            (mkName "fmapSum", ExprHash 4),
            (mkName "listUncons", ExprHash 5),
            (mkName "compose", ExprHash 6),
            (mkName "snd", ExprHash 7),
            (mkName "listHead", ExprHash 8),
            (mkName "listTail", ExprHash 9),
            (mkName "list", ExprHash 10),
            (mkName "id", ExprHash 11),
            (mkName "maybe", ExprHash 14)
          ]

unsafeGetExpr' :: Text -> Bindings -> StoreExpression
unsafeGetExpr' input bindings' =
  case parseExpr input of
    Right expr' -> StoreExpression bindings' expr'
    a -> error $ "Error evaluating " <> T.unpack input <> ": " <> show a

unsafeGetExpr :: Text -> StoreExpression
unsafeGetExpr input = unsafeGetExpr' input mempty
